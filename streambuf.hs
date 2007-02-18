{-
  streambuf.hs

  An asynchronous I/O stream buffer.
-}

module Streambuf where

import Control.Exception
-- import Control.Concurrent
import Control.Monad.RWS
import Control.Monad.Cont
import Data.Char
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.C.Error
import System.IO
import System.IO.Error
import Debug.Trace
-- import WordCounting

----- Fundamental I/O Primitives

type Byte     = Word8
type BytePtr  = Ptr Byte
type ByteSize = CSize
type Iovec    = (BytePtr, ByteSize)

toChar :: Byte -> Char
toChar = chr . fromIntegral

toByte :: Char -> Byte
toByte = fromIntegral . ord

toInt :: ByteSize -> Int
toInt = fromIntegral

toSize :: Int -> ByteSize
toSize = fromIntegral

-- wild theory

shiftT :: Monad m => ((a -> ContT r m s) -> ContT s m s) -> ContT s m a
shiftT e = ContT $ \k -> e (lift . k) `runContT` return

resetT :: Monad m => ContT a m a -> ContT r m a
resetT e = lift $ e `runContT` return

-- A CoIO computation represents a value of type @r@ waiting to be
-- processed.

type CoIO r = ContT r IO

type STT st = CoIO st st

run :: MonadIO m => STT st -> m st
run f = liftIO $ runContT f return

alloc :: IO a                   -- construct a
      -> (a -> IO ())           -- destruct a
      -> CoIO r a               -- a waiting for computation to yield r
alloc c d = withContT (\f _ -> bracket (trace "cons" c) (trace "dest" d) (trace "run" f)) (return ())

check_order :: CoIO () ()
check_order = alloc cons dest >> lift (print "foo")
  where cons   = print "before foo"
        dest _ = print "after foo"

data Buffer = Buf !BytePtr !ByteSize !ByteSize !(ByteSize -> CoIO Buffer ByteSize)

withBuf :: ByteSize -> (Buffer -> CoIO Buffer ByteSize) -> CoIO Buffer ByteSize
withBuf n f = callCC (\next -> alloc (mallocArray (toInt n)) free >>= \p -> f (Buf p n 0 (goOn next)))
  where
    goOn :: (ByteSize -> CoIO Buffer ByteSize) -> ByteSize -> CoIO Buffer ByteSize
    goOn f' n' = trace ("realloc to " ++ show n') (withBuf n' f  >>= f')

toBuffer :: ByteSize -> CoIO r Buffer
toBuffer n = withContT (\f _ -> allocaArray (toInt n) (\p -> f (Buf p n 0 undefined))) (return ())

toString :: CoIO r CStringLen -> CoIO r String
toString = withContT (\f buf -> peekCStringLen buf >>= f)

----- abstract devices

class Device a where
  send    :: a -> Iovec -> IO ()
  receive :: a -> Iovec -> IO ByteSize

instance Device Handle where
  send h (ptr, cap)
                = assert (ptr /= nullPtr) $
                    assert (cap > 0) $
                      hPutBuf h ptr (toInt cap)

  receive h (ptr, cap)
                = assert (ptr /= nullPtr) $
                    assert (cap > 0) $ do
                    rc <- tryRead
                    if rc > 0 then return (toSize rc) else do
                      more <- waitForInput
                      if not more then return 0 else do
                        rc' <- tryRead
                        assert (rc' > 0) return (toSize rc')
    where
      tryRead :: IO Int
      tryRead      = throwErrnoIfRetry (<0) "receive" $
                       hGetBufNonBlocking h ptr (toInt cap)

      waitForInput :: IO Bool
      waitForInput = catchJust
                       (\e -> ioErrors e >>= guard . isEOFError)
                       (hWaitForInput h (-1))
                       (\_ -> return False)


slurp :: (MonadIO m, Device d, Storable a) => d -> (Ptr a -> Int -> m Int) -> m ()
slurp = undefined

----- Buffering

type BasePtr = ForeignPtr Byte

data Iobuf = IOB !ByteSize      -- buffer size
                 !ByteSize      -- unused space at the front
                 !BytePtr       -- begin of memory buffer
                 !ByteSize      -- content length

saneIOB :: Iobuf -> Iobuf
saneIOB iob@(IOB cap gap ptr len) =
  assert (ptr /= nullPtr) $
  assert (cap > 0) $
  assert (cap >= len + gap) $
  iob

type StreamBuf = StateT Iobuf

bptr, eptr :: (Monad m) => StreamBuf m BytePtr
bptr = gets $ \(IOB _ gap p _) -> p `plusPtr` toInt gap
eptr = gets $ \(IOB _ gap p l) -> p `plusPtr` toInt (gap + l)

freeSpace, dataLen :: (Monad m) => StreamBuf m ByteSize
freeSpace = gets $ \(IOB cap gap _ len) -> cap - gap - len
dataLen   = gets $ \(IOB  _   _  _ len) -> len

flushGap :: (MonadIO m) => StreamBuf m ()
flushGap = gets saneIOB >>= f
  where
  f (IOB  _  0   _   _ ) = return ()
  f (IOB cap _  ptr len) = do
    p <- bptr
    liftIO $ copyBytes ptr p (toInt len)
    put (IOB cap 0 ptr len)

drain :: (Monad m) => ByteSize -> StreamBuf m ()
drain i = modify (f . saneIOB)
  where
  f (IOB cap gap ptr len) =
    assert (i <= len) $
    case (gap + i, len - i) of
      ( _  ,  0  ) -> IOB cap  0   ptr  0
      (gap', len') -> IOB cap gap' ptr len'

isFull, isEmpty :: (Monad m) => StreamBuf m Bool
isFull  = gets $ \(IOB cap _ _ len) -> cap == len
isEmpty = gets $ \(IOB  _  _ _ len) -> len == 0

-- verb :: String -> ShowS
-- verb = showString
--
-- moreInput :: (MonadIO m) => Handle -> Timeout -> StreamBuf m Bool
-- moreInput h tout = do
--   whenM isFull $ do
--       buf <- get
--       let ioe' = mkIOError userErrorType "moreInput" (Just h) Nothing
--           ioe  = ioeSetErrorString ioe' msg
--           msg  = verb "IOBuf overflow on " $ show buf
--       liftIO (ioError ioe)
--   whenM (liftM (==0) freeSpace) flushGap
--   n <- freeSpace
--   assert (n > 0) $ do
--   pEnd <- eptr
--   i <- liftIO $ do
--     let tryRead = throwErrnoIfRetry (<0) "moreInput" $
--                     hGetBufNonBlocking h pEnd (toInt n)
--     rc <- tryRead
--     if rc > 0 then return (fromIntegral rc) else do
--       more <- waitForInput h tout
--       if not more then return 0 else do
--         rc' <- tryRead
--         assert (rc' > 0) return (fromIntegral rc')
--   if i == 0 then return False else do
--     IOB cap gap ptr len <- gets saneIOB
--     put (IOB cap gap ptr (len + i))
--     return True
--
-- whenM :: (Monad m) => m Bool -> m () -> m ()
-- whenM b f = b >>= \b' -> when b' f
--
-- whileM :: (Monad m) => m Bool -> m () -> m ()
-- whileM b f = whenM b (f >> whileM b f)
--
-- devNull :: (MonadIO m) => StreamBuf m ()
-- devNull = whileM (moreInput stdin (-1)) (dataLen >>= drain)
--
-- beLikeDevNull :: IO ()
-- beLikeDevNull = allocaIOB 4096 (evalStateT devNull)
--
-- runIOB :: Capacity -> StreamBuf (StateT st IO) () -> st -> IO st
-- runIOB cap f st =
--   allocaIOB cap $ \iob -> execStateT (evalStateT f iob) st
--
-- wcST :: Buffer -> StateT WordCount IO ()
-- wcST (p,n) = get >>= lift . wcBuffer (p,n) >>= put
--
-- getBuffer :: (Monad m) => StreamBuf m Buffer
-- getBuffer = liftM2 (\p n -> (p,n)) bptr dataLen
--
-- wcIOB :: Handle -> IO WordCount
-- wcIOB h = runIOB 4096 (whileM driver handler) initWC
--   where
--   driver  = moreInput h (-1)
--   handler = do buf@(_,n) <- getBuffer
--                lift (wcST buf)
--                drain n
--
-- safeWrite :: (Handle -> IO ()) -> Handle -> Timeout -> IO ()
-- safeWrite f h tout = timeout tout (f h) >>= maybe err return
--   where
--   err  = liftIO (ioError ioe)
--   ioe' = mkIOError userErrorType "safeWrite" (Just h) Nothing
--   ioe  = ioeSetErrorString ioe' msg
--   msg  = verb "write timeout after " . shows tout $ " milliseconds"
--
-- spew :: (MonadIO m) => Handle -> Timeout -> String -> m ()
-- spew h tout msg = liftIO $ safeWrite (\_ -> hPutStr h msg) h tout
--
-- spewBuf :: (MonadIO m) => Handle -> Timeout -> Buffer -> m ()
-- spewBuf h tout (p,n) =
--   liftIO $ safeWrite (\_ -> hPutBuf h p (toInt n)) h tout
--
-- consumeLine :: (MonadIO m) => (String -> m ()) -> StreamBuf m Bool
-- consumeLine f  = getBuffer >>= loop [] 0
--   where
--   loop acc i (p,n) = acc `seq` i `seq` p `seq` do
--     if i == n then return False else do
--       let i' = assert (i < n) (i + 1)
--       c <- liftIO $ fmap toChar (peek p)
--       if (c /= '\n')
--          then loop (c:acc) i' (p `plusPtr` 1, n)
--          else do lift $ f (reverse (c:acc))
--                  drain i'
--                  return True
--
-- consumeLines :: (MonadIO m) => (String -> m ()) -> StreamBuf m ()
-- consumeLines f  = whileM (consumeLine f) (return())
--
-- revIOB :: IO ()
-- revIOB = allocaIOB 4096 $ evalStateT driver
--   where
--   driver  = whileM (moreInput stdin (-1)) (consumeLines handler)
--   handler = hPutStr stdout . reverse
--
-- onError :: (MonadError e m) => (e -> Bool) -> a -> m a -> m a
-- onError isE a f            = catchError f errh
--   where errh e | isE e     = return a
--                | otherwise = throwError e
--
