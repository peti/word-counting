{-
  iodriver.hs

  Wild experiment.
-}

module Main where

import Control.Arrow
import Control.Monad.Error
import Control.Monad.RWS
import Control.Exception
import System.IO
import System.IO.Error
import Foreign
import Foreign.C.Error
import Foreign.C.String
import Data.Char
import WordCounting
import Types

-- Primitive Types

data Iobuf = Iobuf !BytePtr     -- ^ base pointer
                   !ByteSize    -- ^ front gap length
                   !ByteSize    -- ^ payload length
                   !ByteSize    -- ^ buffer capacity
             deriving (Show, Eq)

drain :: Iobuf -> ByteSize -> Iobuf
drain iob@(Iobuf bptr gap len cap) n
  | n == 0    = iob
  | n < len   = Iobuf bptr (gap + n) (len - n) cap
  | n == len  = Iobuf bptr 0 0 cap
  | otherwise = error (showString "illegal drain " . shows n . showString " " $ show iob)

-- I/O Devices

class IOSink a where
  sendMsg :: a -> Iovec -> IO ByteSize

class IOSource a where
  recvMsg :: a -> Iovec -> IO ByteSize

instance IOSink Handle where
  sendMsg h (Iovec bptr cap)
    | bptr == nullPtr   = fail "sendMsg: no nullPtr"
    | cap == 0          = fail "sendMsg: no null buffer"
    | otherwise         = tryWrite False bptr cap >>= maybeRetryWrite

    where
      tryWrite :: Bool -> BytePtr -> ByteSize -> IO ByteSize
      tryWrite force p n
                        = fmap toSize $
                            throwErrnoIfRetry (<0) "sendMsg" $
                               case force of
                                 True  -> hPutBuf h p 1 >> return 1
                                 False -> hPutBufNonBlocking h p (toInt n)

      maybeRetryWrite :: ByteSize -> IO ByteSize
      maybeRetryWrite 0
        | cap == 1      = tryWrite True bptr cap
        | otherwise     = do tryWrite True bptr cap
                             fmap (+1) (tryWrite False (bptr `plusPtr` 1) (cap - 1))
      maybeRetryWrite i = return i

instance IOSource Handle where
  recvMsg h (Iovec p n)
    | p == nullPtr      = fail "recvMsg: no nullPtr"
    | n == 0            = fail "recvMsg: no null buffer"
    | otherwise         = catchJust
                            (\e -> ioErrors e >>= guard . isEOFError)
                            (tryRead >>= maybeRetryRead)
                            (\_ -> return 0)
    where
      tryRead           :: IO ByteSize
      tryRead           = fmap toSize $
                            throwErrnoIfRetry (<0) "recvMsg" $
                              hGetBufNonBlocking h p (toInt n)

      maybeRetryRead    :: ByteSize -> IO ByteSize
      maybeRetryRead 0  = hWaitForInput h (-1) >>= sndTry
      maybeRetryRead i  = return i

      sndTry            :: Bool -> IO ByteSize
      sndTry False      = return 0
      sndTry True       = tryRead

----- I/O Drivers

data SP m i o = Put !o !(SP m i o)
              | Get !(i -> m (SP m i o))
              | Block !(m (SP m i o))

instance Monad m => Arrow (SP m) where
  Put i sp1 >>> Get sp2         = Block (liftM (sp1 >>>) (sp2 i))
  sp1       >>> Put o sp2       = Put o (sp1 >>> sp2)
  Get sp1   >>> sp2             = Get (\i -> liftM (>>> sp2) (sp1 i))
  sp1       >>> Block sp2       = Block (liftM (sp1 >>>) sp2)
  Block sp1 >>> sp2             = Block (liftM (>>> sp2) sp1)
  arr f                         = Get (\i -> return (Put (f i) (arr f)))
  first                         = bypass []
    where
      bypass :: Monad m => [c] -> SP m a b -> SP m (a,c) (b,c)
      bypass ds     (Get sp)    = Get (\(b,d) -> liftM (bypass (ds ++ [d])) (sp b))
      bypass (d:ds) (Put c sp)  = Put (c,d) (bypass ds sp)
      bypass []     (Put c sp)  = Get (\(_,d) -> return (Put (c,d) (bypass [] sp)))
      bypass ds     (Block sp)  = Block (liftM (bypass ds) sp)

instance MonadPlus m => ArrowZero (SP m) where
  zeroArrow = Get (\_ -> mzero)

runSP :: (Monad m) => SP m () () -> m ()
runSP (Block sp) = sp >>= runSP
runSP (Put _ _)  = return ()
runSP (Get _)    = return ()

pushSP :: (Monad m) => [o] -> SP m i o -> SP m i o
pushSP [] sp     = sp
pushSP (x:xs) sp = Put x $ pushSP xs sp

byLine :: (MonadIO m) => SP m Iovec String
byLine = wait []
  where
    wait buf             = Get $ \(Iovec p n) -> run buf p n

    run buf          _ 0 = return (wait buf)
    run buf@('\n':_) p n = liftM (Put (reverse buf)) (run [] p n)
    run buf          p n = do c <- liftIO (fmap castCCharToChar (peek (castPtr p)))
                              run (c:buf) (p `plusPtr` 1) (n - 1)

readerSP :: (IOSource a, MonadIO m, MonadPlus m) => a -> Iovec -> SP m () Iovec
readerSP dev iov@(Iovec p _) = Block $ do
  i <- liftIO (recvMsg dev iov)
  case i of 0 -> return zeroArrow
            _ -> return (Put (Iovec p i) (readerSP dev iov))

writerSP :: (IOSink a, MonadIO m) => a -> SP m Iovec ()
writerSP dev = Get sp
  where
    sp (Iovec _ 0)      = return (writerSP dev)
    sp iov@(Iovec p n)  = do i <- liftIO (sendMsg dev iov)
                             sp (Iovec (p `plusPtr` toInt i) (n - i))

test :: (MonadIO m, MonadPlus m) => Iovec -> SP m () ()
test iov = readerSP stdin iov >>> writerSP stdout

whileTrue :: (Monad m) => m Bool -> m () -> m ()
whileTrue c f = c >>= \b -> when b (f >> whileTrue c f)

drive :: (MonadIO m, IOSource d) => (ByteSize -> Iobuf -> m Iobuf) -> d -> Iobuf -> m Iobuf
drive f dev (Iobuf bptr gap len cap) = do
  i <- liftIO $ recvMsg dev (Iovec (bptr `plusPtr` toInt(gap + len)) (cap - gap - len))
  iob' <- f i (Iobuf bptr gap (len + i) cap)
  case i of
    0 -> return iob'
    _ -> drive f dev iob'

consumeNew :: (MonadIO m) => (ByteSize -> Iovec -> m ByteSize)
                          -> (ByteSize -> Iobuf -> m Iobuf)
consumeNew f i iob@(Iobuf bptr gap len _) = f i iov >>= return . drain iob
  where
    iov = Iovec (bptr `plusPtr` toInt gap) len

consumeSome :: (MonadIO m) => (Iovec -> m ByteSize)
                           -> (ByteSize -> Iobuf -> m Iobuf)
consumeSome = consumeNew . const

consume :: (MonadIO m) => (Iovec -> m ())
                       -> (ByteSize -> Iobuf -> m Iobuf)
consume f = consumeSome (\iov@(Iovec _ n) -> f iov >> return n)

runSP' :: (MonadIO m, IOSource d) => (Iovec -> m ByteSize) -> d -> Iobuf -> m Iobuf
runSP' = drive . consumeSome

slurp :: (MonadIO m, IOSource d) => (Iovec -> m ()) -> d -> Iobuf -> m Iobuf
slurp = drive . consume

wcIovec :: (MonadIO m) => Iovec -> StateT WordCount m ()
wcIovec iov = get >>= iter iov >>= put
  where
    iter :: MonadIO m => Iovec -> WordCount -> m WordCount
    iter (Iovec _ 0) st@(WC _ _ _ _) = return st
    iter (Iovec p i) st@(WC _ _ _ _) = do
      c <- liftIO (peek p) >>= return . chr . fromIntegral
      iter (Iovec (p `plusPtr` 1) (i - 1)) (wc c st)

copyStream :: (IOSource a, IOSink b) => a -> b -> Iovec -> IO Iobuf
copyStream a b (Iovec p n) = runSP' (sendMsg b) a (Iobuf p 0 0 n)

main :: IO ()
main = do
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering

  let bufsize :: ByteSize
      bufsize = 4096

  -- run SP

  allocaArray (toInt bufsize) $ \p ->
    runSP (test (Iovec p bufsize))

  putStrLn "done"

{-
  -- count words

  WC _ l w c <- allocaArray (toInt bufsize) $ \p ->
                  flip execStateT initWC $
                    slurp wcIovec stdin (Iobuf p 0 0 bufsize)
  putStrLn . shows l . (' ':) . shows w . (' ':) $ show c
-}
  -- done

  return ()


----- tools

onError :: (MonadError e m) => (e -> Bool) -> a -> m a -> m a
onError isE a f            = catchError f errh
  where errh e | isE e     = return a
               | otherwise = throwError e



-- ----- Configure Emacs -----
--
-- Local Variables: ***
-- haskell-program-name: "ghci -lcrypto" ***
-- End: ***
