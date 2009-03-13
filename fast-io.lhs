Fast I/O in Haskell
===================

:Author: Peter Simons <simons@cryp.to>
:Date:   2005-10-15

.. contents::

::

> module Main ( main ) where
>
> import Control.Exception ( assert )
> import Control.Monad.Error
> import Control.Monad.State
> import System.Timeout
> import Data.Char ( chr )
> import Data.List ( isPrefixOf )
> import Foreign
> import Foreign.C.Error ( throwErrnoIfRetry )
> import System.IO
> import System.IO.Error
> import System.Environment ( getArgs )

Introduction
------------

When designing computer software, it is often difficult to
determine the right amount of abstraction. If your design
turns out to be too abstract for the problem you're trying
to solve, then your program will be slow and inefficient. If
your design does not abstract its components enough,
however, your software will be difficult to extend and very
hard to re-use.

Haskell's I/O facilities suffer from the same phenomenon. On
one hand we have an input/output API that maps directly to
``read(2)`` and ``write(2)`` as defined by [POSIX]_::

  hGetBuf :: Handle -> Ptr a -> Int -> IO Int
  hPutBuf :: Handle -> Ptr a -> Int -> IO ()

These functions provide no abstraction whatsoever. They may
fail with all kinds of error conditions, and they force us
to worry about memory buffer management; something we don't
like very much in Haskell, because doing it securely in the
presence of asynchronous exceptions is difficult.

On the other hand there is the lazily-evaluated interface::

  hGetContents :: Handle -> IO String
  hPutStr      :: Handle -> String -> IO ()

These functions abstract I/O completely away. There is no
such thing as an ordered stream: if we want to, we can
``reverse`` our input and process it backwards without a
shrug -- because of lazy evaluation, everything is possible.

After observing the run-time performance and memory
requirements of many applications written in Haskell, there
is a growing suspicion that the lazy API might be *too*
abstract. It is hard to design efficient software while
treating all I/O as lazily evaluated lists of characters.
The temptation is too great, so to speak. The following
innocent impersonation of the standard Unix utility
``wc(1)`` demonstrates this nicely::

> wcLazy :: IO ()
> wcLazy = do
>   file <- hGetContents stdin
>   putStrLn $ show (length (lines file)) ++ " " ++
>              show (length (words file)) ++ " " ++
>              show (length file)

The expressiveness of the program is amazing, it really is
quite obvious what it will do when run. The only problem is
that, when run, the compiled version of this program is
approximately 39 times slower than the original::

  $ time ./wcLazy </usr/share/dict/words
  234937 234937 2486824

  real  0m2.801s
  user  0m2.723s
  sys   0m0.075s

  $ time /usr/bin/wc </usr/share/dict/words
  234937 234937 2486824

  real  0m0.071s
  user  0m0.058s
  sys   0m0.009s

Arguably, the odd characteristics of the ``words`` file
might be to blame, but still this result is unsatisfactory.
The program spends way too much time worrying how to
evaluate itself and way too little time incrementing those
three integers we are trying to compute. Our algorithm
doesn't specify any evaluation order, hence the program
doesn't have any.

Unfortunately, fast I/O is all about evaluation order. An
input buffer the application has received must be evaluated
completely by the time the *next* input buffer arrives. How
to write efficiently is somewhat less clear because output
is usually bound to input, but the same principle applies:
if you have data that can be written right now, you should
write it *right now*. Compared to the average CPU these
days, I/O is incredibly slow, so I/O should never have to
wait on the application. We have to evaluate our
computations strictly bound to the actual input/output
stream.

It may sound difficult to achieve in a non-strict language
like Haskell, but curiously enough the opposite is true. A
tool to encode this evaluation order into our programs is
readily available, waiting to be used: the monad.

The remainder of this article is dedicated to developing a
monadic interface to I/O on static memory buffers. We go
this road this by speeding ``wcLazy`` up a little first, and
then generalizing the lessons learnt from this fine
intellectual achievement into a monadic state transformer
aptly called ``StreamBuf``, which represents a »window« into
an I/O stream at a given point of evaluation.

This text is meant to be an introduction into how to perform
efficient I/O in Haskell, written for practical programmers.
This article owes its existence to Shae Matijs Erisson's
very much appreciated continued encouragement.


Word-Counting On Steroids
-------------------------

Our first attempt at re-implementing ``wc(1)`` was
beautiful, but inefficient. The main flaw of ``wcLazy`` is
that it specifies the computation over the entire input
string instead of *iterating* over the *stream* of input
characters. To speed the program up, we need a word-counting
function that can process the input stream strictly one
character after another. Defining such a function is
surprisingly simple, given an appropriate data type to
represent the state of the counting process::

> data WordCount
>   = WC !Bool         -- have seen whitespace
>        !Int          -- lines
>        !Int          -- words
>        !Int          -- characters
>     deriving (Show)
>
> initWC :: WordCount
> initWC = WC True 0 0 0

Now we can process a character stream as follows: we
increment the character counter every time, obviously; we
increment the line counter every time we see a ``'\n'``
linefeed; and every time we see whitespace (including
linefeed), we set the boolean to ``True``, so that the next
non-whitespace character increments the word counter and
resets the boolean to ``False``. The first non-whitespace
character following one or more whitespaces starts a new
word::

> wc :: Char -> WordCount -> WordCount
> wc '\n' (WC _     l w c) = WC True (l+1)  w   (c+1)
> wc ' '  (WC _     l w c) = WC True   l    w   (c+1)
> wc '\t' (WC _     l w c) = WC True   l    w   (c+1)
> wc  _   (WC True  l w c) = WC False  l  (w+1) (c+1)
> wc  _   (WC False l w c) = WC False  l    w   (c+1)

The nature of this function's evaluation is made rather
obvious by its type signature: once a character has been
read and accounted for, it can be thrown away. This is
important, because it allows our ``wc(1)`` clone to have
constant space requirements, regardless of the size of its
input file.

The ``wc`` function is extremely fast because it is
completely strict. In the definition of ``WordCount``, all
fields were marked as being strict by placing the ``!``
annotation before their types. For the compiler, this means
that the values of those fields should always be evaluated
when ``WordCount`` is. ``WordCount`` is evaluated every time
it is bound through its constructor. The difference to a
data type made up of non-strict fields is that binding a
``WordCount`` as ``(WC _ _ _ _)`` will suffice to evaluate
all members -- something our ``wc`` function always does, so
there is no laziness involved.

For the magic to work, however, you must ensure that the
``WordCount`` state *remains* strict when you use the
function in combinators. It is tempting to define a
combinator for processing lists of characters as::

  wcList :: WordCount -> [Char] -> WordCount
  wcList = foldr wc

By using the lazy combinator, however, our ``String``
variant of ``wc`` would become lazy too. Put to work on a
fairly large input, it would probably simply exceed the
stack. The supposedly strict folding combinator ::

  wcList = foldl' (flip wc)

is somewhat better, but *de facto* it loses most of ``wc``'s
performance too. To process an input string instead of a
single character, it is thus best to spell the recursion out
explicitly::

> wcList :: WordCount -> [Char] -> WordCount
> wcList st@(WC _ _ _ _) []     = st
> wcList st@(WC _ _ _ _) (c:cs) = wcList (wc c st) cs
>
> wcLazy' :: IO ()
> wcLazy' = getContents >>= print . wcList initWC

This version of our word-counting program produces the
following run-time result::

  $ time ./wcLazy' </usr/share/dict/words
  WC True 234937 234937 2486824

  real  0m0.310s
  user  0m0.255s
  sys   0m0.052s

It is approximately 4 times slower than the C version, and
this overhead really is the result of using ``getContents``
because ``wcList`` can certainly process the input as fast
as the hard-disk can deliver it. If we want to increase
performance even more, we have to write a ``wc`` version
that can process a raw memory buffer, because then we can
use the low-level API to I/O to drive it. In Haskell, a
memory buffer is usually represented as a tuple of »base
pointer« and »buffer length«, which gives the following
trivial wrapper::

> wcBuffer :: (Ptr Word8, Int) -> WordCount -> IO WordCount
> wcBuffer (_, 0) st@(WC _ _ _ _) = return st
> wcBuffer (p, n) st@(WC _ _ _ _) = do
>   c <- fmap (chr . fromIntegral) (peek p)
>   wcBuffer (p `plusPtr` 1, n - 1) (wc c st)

We simply map the numerical value of the current ``Word8``
to ``Char`` before feeding it into ``wc``; no charset
decoding takes place. Again, this function is strict in its
arguments and result, its translation into machine code
should be *very* straight-forward.

The only task left to accomplish is to define an appropriate
I/O driver for this function. Its structure will look oddly
familiar to C programmers. First of all, we have to allocate
a memory buffer for reading::

> wcHandle :: Handle -> IO WordCount
> wcHandle h = allocaArray 4096 (\p -> loop p initWC)
>   where
>   loop ptr st@(WC _ _ _ _) = do

Now we have the main loop to iterate over the stream of
input buffers all the while passing on the state -- the
``WordCount`` -- which will be our result when the input
stream ends::

>     rc <- hGetBuf h ptr 4096
>     if rc <= 0
>        then return st
>        else wcBuffer (ptr, rc) st >>= loop ptr
>

For the sake of simplicity, a buffer size of 4 KBytes is
hard-coded into the function. The standard function
``allocaArray`` has the nice property of allocating the
memory on stack, so we don't have to free it. For larger
buffers, the same effect can be achieved by creating an
allocation/de-allocation scope around another function with
``bracket``::

  withArray :: Int -> ((Ptr Word8, Int) -> IO a) -> IO a
  withArray n f = bracket (mallocArray n) (free)
                          (\p -> f (p,n))

Note that the buffer will be gone once ``f`` returns -- the
same is true when using ``allocaArray``. These buffers can
be considered *static* because you cannot safely enlarge or
replace them while ``f`` evaluates. We will come back to
this topic later in the text. Right now, we are interesting
in ``wcHandle``'s performance when driven by the main
program ``(wcHandle stdin >>= print)``::

  $ time ./wcHandle </usr/share/dict/words
  WC True 234937 234937 2486824

  real  0m0.049s
  user  0m0.035s
  sys   0m0.011s

In all fairness to the C implementation, which offers far
more functionality, our version can be said to be equally
fast as the C program is. The point is that we *can* process
large streams of input data in Haskell just fine.

The structure of ``wcHandle`` guarantees the program's
evaluation order: its evaluation is determined entirely by
the arrival of input data -- the program is highly
responsive to I/O. Now, we don't want to write code like we
used to in C, but clearly the approach employed by
``wcHandle`` has its merits. So we solve the dilemma the
functional way: we write the C-like code *once*, and
abstract it through a monadic interface.


Generalizing ``wcHandle``
-------------------------

When we look closely at ``wcHandle``, we notice that it
could be made more generic by passing the buffer-handling
function as a parameter, instead of hard-coding ``wcBuffer``
into the I/O loop. Furthermore, we could require buffer size
and initial state as arguments too, thus defining the
generic input driver::

  type BlockHandler st = (Ptr Word8, Int) -> st -> IO st

  runLoop
    :: Handle           -- input handle
    -> Int              -- buffer capacity
    -> BlockHandler st  -- handler function
    -> st               -- initial state
    -> IO st            -- state at time of EOF

A combinator like ``runLoop`` can be used to iterate any
given stateful computation over an input stream, and it does
so *fast*.

However, it turns out ``runLoop`` is unsatisfactory for
several reasons. For one, it assumes that the handler
function can process the entire input buffer at once. This
is true for a simple algorithm like word counting, but other
tasks may require a complete *line* of input to work --
almost every Internet protocol does, for example. Assuming
we would receive the input buffer ``"complete
line\nincomplete"``, then a line-based handler function
would want to consume the line, but *not* the rest. When
driven by ``runLoop``, however, this handler function would
have to implement a buffering strategy on top of the static
buffer we already have, because the next ``hGetBuf`` call
will most likely overwrite the ``"incomplete"`` bit.
Clearly, our handler function needs more control over the
buffer, so that it can consume a part of the input, or even
modify the buffer's contents as it sees fit.

One more problem is that ``runLoop`` knows no other way of
operating than to consume the entire contents of the input
stream -- much like ``hGetContents`` does. The handler
function cannot terminate the input loop other than by
throwing an exception. ``runLoop``'s single-mindedness makes
it very hard to design applications which have to consider
more than one input/output stream, where you might want to
re-use the same buffer to read from a different ``Handle``
every time, etc.

One last problem is that ever since the advent of the
Internet, I/O really means *network* I/O. Real-life
applications need the ability to specify timeouts for I/O
operations. Furthermore, we cannot use ``hGetBuf`` to read
from a network socket, because the function blocks. When
``hGetBuf`` is told to read 4096 bytes, it will not return
until it has read 4096 bytes or until EOF ensues. Consider
an application which implements a line-based dialogue over
the Internet, like an SMTP server. When such an application
receives the input ::

  HELO mx.example.org\r\n

then it must *respond* before there will be any more input
to read. The call to ``hGetBuf``, however, will not return!
That line may be a complete input statement, but it is
shorter than 4096 bytes. A line-based dialogue cannot be
implemented with ``hGetBuf`` -- other than reading character
by character. Neither could an HTTP server, by the way. The
valid request ::

  GET /index.html HTTP/1.1\r\n
  Host: www.example.org\r\n
  \r\n

is too short, too.

This leaves us in an awkward position, because the only
Haskell implementation that offers non-blocking I/O is the
Glorious Haskell Compiler [GHC]_ which provides the
functions ::

  hGetBufNonBlocking :: Handle -> Ptr a -> Int -> IO Int
  hPutBufNonBlocking :: Handle -> Ptr a -> Int -> IO Int
  hWaitForInput      :: Handle -> Int -> IO Bool

as part of its ``System.IO`` module. These functions will
perform as much I/O as they can, and then they'll return how
many bytes were read or written respectively. The
computation ::

  rc <- hGetBufNonBlocking stdin ptr 4096

may read *up to* 4096 bytes into the buffer, but on
inspection of ``rc`` it might also turn out that it read
*zero* bytes. Therefore, you need to call ``hWaitForInput``
explicitly in case you do want to block until more input is
available.

It is odd that the blocking I/O functions ``hGetBuf`` and
``hPutBuf`` appear to be the »standard versions« in most
people's eyes, even though a non-blocking function can be
trivially wrapped to block, but the opposite is not true.
Technically, the non-blocking versions should be primitive,
and »blocking« should be implemented by a generic
combinator.

Anyway, as it is, we are stuck on GHC for the moment, but
then everyone writing production code in Haskell is using
GHC anyway, so the limitation isn't that serious in
practice. After all, the best way of encouraging other
implementations to provide non-blocking I/O too is to use
it.

Before we do, we need a better understanding of the concept
of a memory buffer though. ``(Ptr Word8, Int)`` may be a
concise representation, but it is not powerful enough to
implement complex algorithms with it.

A General-Purpose I/O Buffer
----------------------------

There are two operations an I/O buffer must support
efficiently: (1) appending data at the end and (2) consuming
data from the front. An input loop, for example, will
usually appended new data it has read between calls to the
handler function, which in turn consumes those chunks it has
processed. When writing, the output loop will consume the
data it has successfully written, while the handler function
appends new output as it is computed.

To support these operations, our data type needs to contain
more information than just »base pointer« and »content
length«. In order to determine how much data can be
appended, for example, we need to know the buffer's
capacity. Similarly, we need to know how many consumed bytes
the buffer contains -- usually called »front gap« -- so that
we can calculate the pointer to the beginning of the
buffer's unconsumed contents::

> type BytePtr   = Ptr Word8
> type ByteCount = Word32
> type Capacity  = ByteCount
>
> data IOBuf =
>   IOB !Capacity   -- size of underlying memory buffer
>       !ByteCount  -- unused space at the front
>       !BytePtr    -- address of the memory buffer
>       !ByteCount  -- length of data starting at (bptr + gap)
>   deriving (Show)

In all likelihood, the fields ``Capacity`` and ``BytePtr``
will remain constant during the life-time of such a buffer;
the only fields which can be expected to change are the
content length and the size of the front gap. In other
words, manipulating the contents of the buffer is, for the
most part, a matter of incrementing or decrementing those
two counters.

Unlike the system API to I/O, we don't represent byte counts
as ``Int`` because ``Int`` is a *signed* integer type.
Negative values don't make any sense in our context though,
so we chose ``Word32`` instead, which is unsigned. Arguably,
in this day and age we could use ``Word64`` for
``ByteCount`` just as well -- most operating systems have
large-file support --, but a continuous block of memory
larger than 4 GBytes can hardly be called an »I/O buffer«
anymore, so the decision was made to stick to ``Word32``,
which has the neat advantage of being a primitive data type
for most CPUs too.

Since Haskell's I/O API *does* specify byte counts in
``Int``, we need a way to convert a ``ByteCount`` to a
signed integer, which is easily accomplished with this short
helper function::

> toInt :: ByteCount -> Int
> toInt = fromEnum

The use of ``fromEnum`` instead of ``fromIntegral`` has the
benefit of catching overflow errors for us, in case ``Int``
cannot represent the ``Word32`` we're trying to convert::

  *Main> toInt (maxBound::Word32)
  *** Exception: Enum.fromEnum{Word32}: value 4294967295
      is outside of Int's bounds (-2147483648,2147483647)

In the same spirit, we also decided that an I/O buffer is
always a buffer of unsigned ``Word8`` octets. We don't even
pretend the buffer's contents would be »characters« in any
shape or form to make it obvious that non-ASCII data usually
needs some form of encoding/decoding stage before it can be
processed. The gory details of this problem are beyond the
scope of this article, though; our I/O code operates only
with *bytes*.

Now a simple wrapper for ``allocaArray`` can create an empty
I/O buffer and run a given computation with it::

> allocaIOB :: Capacity -> (IOBuf -> IO a) -> IO a
> allocaIOB cap f =
>   assert (cap > 0) $
>   allocaArray (toInt cap) (\p -> f (IOB cap 0 p 0))

Although ``allocaArray`` would accept a requested buffer
size of 0, we do not, because the created ``IOBuf`` would be
useless for all practical purposes. We use ``assert`` to
test for this error condition because a statement like
``allocaIOB 0`` can be considered a *program error*, not a
run-time error. In fact, we know several other properties an
``IOBuf`` must fulfill for it to be valid::

> saneIOB :: IOBuf -> IOBuf
> saneIOB iob@(IOB cap gap ptr len) =
>   assert (ptr /= nullPtr) $
>   assert (cap > 0) $
>   assert (cap >= len + gap) $
>   iob

It is generally a good idea to use ``assert`` liberally to
verify the correctness of your data structures because those
checks will uncover program errors very reliably. When
compiling with optimization enabled though, the compiler
removes assertions from the code; so the final version of
the program will suffer no run-time overhead.

The ``StreamBuf`` Monad
-----------------------

With ``IOBuf`` defined, the only thing left to do is to
figure out how to manipulate it comfortably and safely, and
as it happens, Haskell knows a concept which is just perfect
for our purposes: the monadic state transformer ``StateT``.

::

> type StreamBuf = StateT IOBuf

According to this definition, a ``StreamBuf IO a`` is a
function in the ``IO`` monad which has an ``IOBuf`` wrapped
into it as state, and which returns a value of type ``a``
when run. Note that the definition does not make any
assumptions about which monad you'll actually use! For our
purposes, any monad ``m`` which is an instance of
``MonadIO`` will be fine. This means that a ``StreamBuf``
can have state *in addition* to the ``IOBuf``. The following
definition, for instance, is perfectly valid::

  type StreamCounter a = StreamBuf (StateT WordCount IO) a

When writing code in the ``StreamCounter`` monad, we have
*both* data types available as state. The standard function
``get`` will give us the outer state ::

  getIOB :: StreamCounter IOBuf
  getIOB = get

while lifting ``get`` from the inner monad will give us the
``WordCount``::

  getWC :: StreamCounter WordCount
  getWC = lift get

``IO`` computations can generally be run with ``liftIO``, no
matter how many levels of lifting would be necessary.

Of course, it is also possible to use ``StreamBuf`` as the
*inner* monad like this::

  type StreamCounter a = StateT WordCount (StreamBuf IO) a

What makes the difference between both definitions is which
state is available in which monad. In any given monad
transformer, the outer monad can access the inner one, but
not vice versa. When it comes to our ``StreamBuf`` monad,
there is no generally valid rule to determine which ordering
is better; whether you want ``StreamBuf`` to be the inner or
outer monad depends entirely an what you are trying to do.
What makes the definition of ``StreamBuf`` appealing is the
very fact that it allows for this flexibility.

Two useful functions we'll need for manipulating the memory
buffer are ``bptr`` and ``eptr``, which return a pointer to
the beginning and to the end of the buffer's contents
respectively::

> bptr, eptr :: (Monad m) => StreamBuf m BytePtr
> bptr = gets $ \(IOB _ gap p _) -> p `plusPtr` toInt gap
> eptr = gets $ \(IOB _ gap p l) -> p `plusPtr` toInt (gap + l)

These functions will always evaluate our ``IOBuf`` because
they bind the data type through its constructor and all
fields of ``IOBuf`` have been declared to be strict. Almost
all functions we'll write have this property, thus ensuring
that code written in the ``StreamBuf`` monad will be
evaluated strictly.

Now that we have access into the buffer, we also need to
know how much free space is available for appending data and
how much data is available for consumption::

> freeSpace, dataLen :: (Monad m) => StreamBuf m ByteCount
> freeSpace = gets $ \(IOB cap gap _ len) -> cap - gap - len
> dataLen   = gets $ \(IOB  _   _  _ len) -> len

Note that ``freeSpace`` does not include the free space we
might have at the buffer's front. The ``ByteCount`` it
returns signifies the number of bytes that can be appended
at ``eptr`` without overflowing the buffer's capacity, which
is the information we are really interested in. The front
gap is basically invisible during the lifetime of an
``IOBuf``. However, we need a way to »flush« a buffer --
meaning that we copy its content downward to the base
pointer, thus reducing the size of the front gap to 0. If we
wouldn't do this, our buffer would probably run out of
``freeSpace`` quickly::

> flushGap :: (MonadIO m) => StreamBuf m ()
> flushGap = gets saneIOB >>= f
>   where
>   f (IOB  _  0   _   _ ) = return ()
>   f (IOB cap _  ptr len) = do
>     p <- bptr
>     liftIO $ copyBytes ptr p (toInt len)
>     put (IOB cap 0 ptr len)

In case the front gap is zero already, we have nothing to
do. Otherwise, we use the standard function ``copyBytes`` to
move the buffer's contents. Testing for a length of zero
before calling ``copyBytes`` would be possible, but it is
unnecessary, because the function we use to manipulate the
front gap handles this case already::

> drain :: (Monad m) => ByteCount -> StreamBuf m ()
> drain i = modify (f . saneIOB)
>   where
>   f (IOB cap gap ptr len) =
>     assert (i <= len) $
>     case (gap + i, len - i) of
>       ( _  ,  0  ) -> IOB cap  0   ptr  0
>       (gap', len') -> IOB cap gap' ptr len'

Draining a negative number of bytes -- thereby *decreasing*
the front gap -- is impossible because of our use of
``Word32`` for ``ByteCount``. It is possible, however, to
drain more bytes than the buffer actually contains; a
programmer error we prevent through the use of ``assert``.
In addition to that, the sanity checks performed by
``saneIOB`` are applied *before* we modify the I/O buffer
too.

Two more simple functions we will need occasionally are::

> isFull, isEmpty :: (Monad m) => StreamBuf m Bool
> isFull  = gets $ \(IOB cap _ _ len) -> cap == len
> isEmpty = gets $ \(IOB  _  _ _ len) -> len == 0

The function ``isFull`` returns ``True`` when there is no
more space in the buffer -- not even a front gap we could
flush. In an input loop, this condition most likely
constitutes a fatal error because we have no way of
enlarging the buffer securely. Consequently, the size of an
``IOBuf`` used for input should be chosen so that it can
contain the largest possible input chunk your handler
function needs to perform an operation that leads to data
being consumed. When implementing an application that reads
lines of input, for example, the size of your I/O buffer
will be the maximum line length your application can handle.

On first sight, this may look like a serious limitation, but
the opposite is true: it is a feature! To see why, consider
the standard function ``hGetLine``::

  hGetLine :: Handle -> IO String

This function allows to read arbitrarily long lines.
Consequently, it will require arbitrarily large amounts of
memory to perform its job! Feeding an application that is
using this function an endless stream of characters, without
ever sending a line-feed, will most likely result in the
process being aborted and may -- depending on its
configuration -- severely disrupt the health of the machine
that's running the program. In practice, applications which
need arbitrarily large blocks of input to do anything useful
are rare, precisely because of the problem of buffer
management. Line-based protocols, for example, always
specify a maximum line length an application has to support
-- which would be the appropriate size of your input buffer
then.

Reading Input
-------------

Haskell's API to reading input is somewhat difficult to use
because the functions mix different kinds of error
reporting. Both ``hGetBuf`` and ``hGetBufNonBlocking``
signify error conditions by returning a negative value and
by throwing exceptions. ``hWaitForInput`` signifies EOF with
an exception too, instead of returning ``False``, like it
should. Then there is the problem that ``hWaitForInput``
promises to support timeouts -- hence the ``Int`` argument
--, but the current implementation really does not: it is
only safe to call the function with a timeout of zero
(return immediately) or less than zero (block indefinitely).

Our approach is to signal all error conditions, including
timeouts, through exceptions -- but never EOF, because it is
not an error condition but a state every ``Handle``
invariably reaches at some point. Our version
``waitForInput`` returns ``False`` on EOF, and when it
returns ``True``, the ``Handle`` is readable. ::

> waitForInput :: Handle -> Timeout -> IO Bool
> waitForInput h tout = do
>   timeout tout $
>     onError isEOFError False $
>       hWaitForInput h (-1)
>   >>= maybe (ioError ioe) return
>   where
>   ioe  = ioeSetErrorString ioe' msg
>   ioe' = mkIOError userErrorType "waitForInput" (Just h) Nothing
>   msg  = verb "read timeout after " . shows tout $ " milliseconds"
>
> verb :: String -> ShowS
> verb = showString

The timeout problem is solved with a sledgehammer: we tell
``hWaitForInput`` to wait indefinitely, and wrap it with a
general-purpose timeout function of our own. Timeouts for
arbitrary ``IO`` computations can be implemented using the
techniques described in Simon Peyton Jones' excellent
[Awkward]_ paper. The function we use is provided by the
[Child]_ package and has the following signature::

  timeout :: Timeout -> IO a -> IO (Maybe a)

How to catch a timeout exception is described in th esection
`Error Handling`_. The same section gives the definition of
the combinator ``onError``, which we use to translate EOF
exceptions into ``False``.

Actually reading from a ``Handle``, once we know we can,
turns out to be a very imperative subject::

> moreInput :: (MonadIO m) => Handle -> Timeout -> StreamBuf m Bool
> moreInput h tout = do
>   whenM isFull $ do
>       buf <- get
>       let ioe' = mkIOError userErrorType "moreInput" (Just h) Nothing
>           ioe  = ioeSetErrorString ioe' msg
>           msg  = verb "IOBuf overflow on " $ show buf
>       liftIO (ioError ioe)
>   whenM (liftM (==0) freeSpace) flushGap

Up to this point, we have assured that there is empty space
in the buffer. The ``whenM`` function used above is a
monadic variant of the standard function ``when`` -- it's
definition will be shown shortly. Having asserted that we
actually *can* read, we figure out how many bytes we can
store, and where we have to read them to::

>   n <- freeSpace
>   assert (n > 0) $ do
>   pEnd <- eptr

Now we'll perform I/O, retrying the operation in case the
``Handle`` isn't readable or in case of an interrupted
system call::

>   i <- liftIO $ do
>     let tryRead = throwErrnoIfRetry (<0) "moreInput" $
>                     hGetBufNonBlocking h pEnd (toInt n)
>     rc <- tryRead
>     if rc > 0 then return (fromIntegral rc) else do
>       more <- waitForInput h tout
>       if not more then return 0 else do
>         rc' <- tryRead
>         assert (rc' > 0) return (fromIntegral rc')

The function ``throwErrnoIfRetry``, which does most of our
error handling, is a convenience wrapper provided by
``Foreign.C.Error``. The result ``i`` signifies the number
of bytes successfully read -- zero meaning EOF, because we
always read *at least* one byte. The only thing left to do
is to add ``i`` to the buffer's content length::

>   if i == 0 then return False else do
>     IOB cap gap ptr len <- gets saneIOB
>     put (IOB cap gap ptr (len + i))
>     return True

There is some fine detail to the usage of this function. For
example, even though ``moreInput`` will perform a
``flushGap`` if necessary, you shouldn't necessarily rely on
it, because ``moreInput`` may flush the gap at the worst
possible time. ``flushGap`` is most efficient when the
buffer is almost empty -- the fewer bytes it contains, the
cheaper the copying process will be. Exactly when this is
the case, however, is something ``moreInput`` cannot know.

On the other hand, flushing the gap even though the buffer
contains a moderately large amount of data may result in
better performance if doing so means that we gain a lot of
free space for the following read. Consider a buffer of 1024
bytes, which contains a front gap of 1022 bytes and a single
byte of data. Clearly it doesn't make much sense to have
``moreInput`` read another single byte into the buffer --
which is all the free space there is. Ultimately, the burden
to decide when the gap should be flushed is on the
programmer.

The definition of the missing ``whenM`` function, as
promised, is::

> whenM :: (Monad m) => m Bool -> m () -> m ()
> whenM b f = b >>= \b' -> when b' f

Given ``whenM``, we can trivially define another monadic
combinator to express looping constructs::

> whileM :: (Monad m) => m Bool -> m () -> m ()
> whileM b f = whenM b (f >> whileM b f)

This function is particularly useful in combination with
``moreInput``, because using it we can express I/O loops
without having to write an explicit recursion. If we wanted
to duplicate the mind-boggling functionality of the Unix
device ``/dev/null``, for example, this is all we'd have to
do::

> devNull :: (MonadIO m) => StreamBuf m ()
> devNull = whileM (moreInput stdin (-1)) (dataLen >>= drain)

Now, there is nothing to unsettle C programmers like
point-free Haskell code. The only problem left to solve is
how to actually *run* that program.

First of all, ``devNull`` will work in any ``MonadIO``, so
it should work in ``IO`` too, which gives the simplified
type signature ``StreamBuf IO ()``. According to our
definition of ``StreamBuf``, this really means ``StateT
IOBuf IO ()``. Knowing the definition of ``StateT`` ::

  newtype StateT s m a =
    StateT { runStateT :: s -> m (a, s) }

we can bind ``devNull`` with ``let StateT f = devNull`` and
have a function ``f`` with the signature::

  IOBuf -> IO ((), IOBuf)

Apparently, ``devNull`` is an ordinary ``IO`` computation
that needs an initialized ``IOBuf`` and returns a modified
``IOBuf``. One way of running ``devNull`` would thus be::

  beLikeDevNull :: IO ()
  beLikeDevNull = do
    let StateT f = devNull
    allocaIOB 4096 f
    return ()

Remember that the created ``IOBuf`` does not survive past
the scope of ``allocaIOB`` -- so we should not return it.

An even simpler version can be defined with the help of the
standard function ``evalStateT``, which runs a ``StateT``
and throws the final state away, returning only the ``a``
value::

> beLikeDevNull :: IO ()
> beLikeDevNull = allocaIOB 4096 (evalStateT devNull)

How about our word-counting program? How would that fit into
the ``StreamBuf`` monad?

Unlike ``devNull``, which doesn't do much except for
throwing away all data, ``wcBuffer`` calculates its result
over the life-time of the entire stream, so its
``WordCount`` state must be passed on in addition to the
``IOBuf``. There are numerous ways to accomplish this task,
but the simplest and most obvious one is to employ the
technique we saw earlier in the definition of the
``StreamCounter`` type: we wrap the ``StreamBuf``
transformer around *another* ``StateT`` monad. In fact, this
approach is common enough to warrant a combinator of its
own::

> runIOB :: Capacity -> StreamBuf (StateT st IO) () -> st -> IO st
> runIOB cap f st =
>   allocaIOB cap $ \iob -> execStateT (evalStateT f iob) st

On first glance, the order of the state values ``iob`` and
``st`` seems to be wrong, but keep in mind that ``StateT``
encodes which value is *missing* to get a computation in the
*underlying* monad. After applying ``iob``, we have a
computation in ``StateT st IO``. After applying ``st`` to
that, we have an ``IO`` computation.

Whether we run the ``StreamBuf`` computation with
``evalStateT``, ``runStateT``, or any other way doesn't
matter in this context, because none of the values it
returns are of any concern to us. All we are interested in
is the state ``st`` of the inner monad. Similarly, the given
computation ``f`` could just as well return an arbitrary
``a`` instead of ``()``. We chose to require it to return
``()`` to make it obvious that the value it returns is never
used anywhere: our I/O computations are all about state and
side-effects.

Before we can run our word-counting function ``wcBuffer`` in
this environment, we need to wrap it into a proper
``StateT``, something that's accomplished fairly easily::

> type Buffer = (BytePtr, ByteCount)
>
> wcST :: Buffer -> StateT WordCount IO ()
> wcST (p,n) = get >>= lift . wcBuffer (p, toInt n) >>= put

Getting a ``Buffer`` inside of a ``StreamBuf`` computation
is just a matter of obtaining the ``bptr`` and ``dataLen``::

> getBuffer :: (Monad m) => StreamBuf m Buffer
> getBuffer = liftM2 (\p n -> (p,n)) bptr dataLen

And now our word-counting ``StreamBuf`` computation is
readily defined as::

> wcIOB :: Handle -> IO WordCount
> wcIOB h = runIOB 4096 (whileM driver handler) initWC
>   where
>   driver  = moreInput h (-1)
>   handler = do buf@(_,n) <- getBuffer
>                lift (wcST buf)
>                drain n

Clearly this function includes a *lot* more logic than our
custom-written ``wc(1)`` variant does, but still the
compiled result compares nicely to ``wcHandle``::

  time ./wcIOB </usr/share/dict/words
  WC True 234937 234937 2486824

  real  0m0.049s
  user  0m0.038s
  sys   0m0.008s


Writing Output
--------------

Compared to reading input efficiently, writing output
efficiently is a piece of cake because GHC already
implements a buffering strategy like we just did; it's just
a matter of switching it on::

  hSetBuffering stdout (BlockBuffering (Just 4096))

A ``Handle`` in ``BlockBuffering`` mode is associated with a
raw memory buffer very similar to our ``IOBuf``. When we say
``hPutStr stdout msg`` now, the implementation will simply
copy the contents of ``msg`` into that buffer. Whenever that
buffer is full, or when ``hFlush`` is called, its contents
will be written to the output stream.


::

> safeWrite :: (Handle -> IO ()) -> Handle -> Timeout -> IO ()
> safeWrite f h tout = timeout tout (f h) >>= maybe err return
>   where
>   err  = liftIO (ioError ioe)
>   ioe' = mkIOError userErrorType "safeWrite" (Just h) Nothing
>   ioe  = ioeSetErrorString ioe' msg
>   msg  = verb "write timeout after " . shows tout $ " milliseconds"
>
> spew :: (MonadIO m) => Handle -> Timeout -> String -> m ()
> spew h tout msg = liftIO $ safeWrite (\_ -> hPutStr h msg) h tout
>
> spewBuf :: (MonadIO m) => Handle -> Timeout -> Buffer -> m ()
> spewBuf h tout (p,n) =
>   liftIO $ safeWrite (\_ -> hPutBuf h p (toInt n)) h tout


Reading Lines
-------------

::

> consumeLine :: (MonadIO m) => (String -> m ()) -> StreamBuf m Bool
> consumeLine f  = getBuffer >>= loop [] 0
>   where
>   loop acc i (p,n) = acc `seq` i `seq` p `seq` do
>     if i == n then return False else do
>       let i' = assert (i < n) (i + 1)
>       c <- liftIO $ fmap (chr . fromIntegral) (peek p)
>       if (c /= '\n')
>          then loop (c:acc) i' (p `plusPtr` 1, n)
>          else do lift $ f (reverse (c:acc))
>                  drain i'
>                  return True
>
> consumeLines :: (MonadIO m) => (String -> m ()) -> StreamBuf m ()
> consumeLines f  = whileM (consumeLine f) (return())
>
> revIOB :: IO ()
> revIOB = allocaIOB 4096 $ evalStateT driver
>   where
>   driver  = whileM (moreInput stdin (-1)) (consumeLines handler)
>   handler = hPutStr stdout . reverse


Error Handling
--------------

::

> type Timeout = Int
>
> onError :: (MonadError e m) => (e -> Bool) -> a -> m a -> m a
> onError isE a f            = catchError f errh
>   where errh e | isE e     = return a
>                | otherwise = throwError e



Command-line Driver
-------------------

::

> main :: IO ()
> main = do
>   hSetBuffering stdin NoBuffering
>   hSetBuffering stdout (BlockBuffering (Just 4096))
>   args <- getArgs
>   case args of
>     "wcLazy"   : _ -> wcLazy
>     "wcHandle" : _ -> wcHandle stdin >>= print
>     "wc"       : _ -> wcIOB stdin >>= print
>     "lrev"     : _ -> revIOB
>     "lrevLazy" : _ -> interact (unlines . map reverse . lines)
>     _              -> fail "usage: [wcLazy,wcHandle,wc,lrev,revLazy}"

References
----------

.. [Haskell] The Haskell Homepage: http://www.haskell.org/

.. [POSIX] The Single UNIX Specification: http://www.opengroup.org/onlinepubs/007908799/index.html

.. [GHC] The Glorious Haskell Compiler: http://www.haskell.org/ghc/

.. [BlockIO] A block-oriented I/O driver for Haskell: http://cryp.to/blockio/

.. [Awkward] Simon Peyton Jones, »Tackling the awkward squad«,
             http://research.microsoft.com/Users/simonpj/papers/marktoberdorf/

.. [Child] Thread Control: http://cryp.to/child/
