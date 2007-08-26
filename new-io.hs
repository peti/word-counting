{-# OPTIONS -farrows #-}

module Main ( main ) where

import Prelude hiding ( null )
import Control.Arrow.SP
import System.IO hiding ( hPutStrLn )
import System.IO.Error
import Control.Exception
import Control.Monad.State
import Control.Monad.Error
import Data.ByteString.Char8
import Data.Monoid
import Foreign
import Timeout
import WordCounting

-- * Error Handling

timeoutErrorType :: IOError
timeoutErrorType = userError "timeout"

isTimeoutError :: IOError -> Bool
isTimeoutError e
  | isUserError e = ioeGetErrorString e == ioeGetErrorString timeoutErrorType
  | otherwise     = False

inputTimeout, outputTimeout :: Handle -> IOError
inputTimeout h  = annotateIOError timeoutErrorType "input" (Just h) Nothing
outputTimeout h = annotateIOError timeoutErrorType "output" (Just h) Nothing

onError :: (MonadError e m) => (e -> Bool) -> a -> m a -> m a
onError isE a f = catchError f (\e -> if isE e then return a else throwError e)

-- * I/O Primitives

type MsecTimeout = Int

timeoutError :: IOError -> MsecTimeout -> IO a -> IO a
timeoutError e to f = timeout to f >>= maybe (ioError e) return

waitForInput :: (MonadIO m) => Handle -> MsecTimeout -> m Bool
waitForInput h to = liftIO $
                      timeoutError (inputTimeout h) to $
                        onError isEOFError False $
                          hWaitForInput h (-1)

-- * Monadic Stream Consumer

ifM :: (Monad m) => m Bool -> (m a, m a) -> m a
ifM cond (true,false) = cond >>= \b -> if b then true else false

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM cond f = ifM cond (f, return ())

whileM :: (Monad m) => m Bool -> m () -> m ()
whileM cond f = whenM cond (f >> whileM cond f)

-- * Stream Processig Arrow

readerSP' :: (Monad m) => m Bool -> m a -> SP m () a -> SP m () a
readerSP' waiter reader eof = Block $
  ifM waiter (liftM (\a -> Put a (readerSP' waiter reader eof)) reader, return eof)

readerSP :: (Monad m, Monoid a) => m Bool -> m a -> SP m () a
readerSP waiter reader = readerSP' waiter reader (Put mempty zeroArrow)

slurpSP :: (MonadIO m) => Handle -> MsecTimeout -> SP m () ByteString
slurpSP h to = readerSP (waitForInput h to) (liftIO (hGetNonBlocking h (8 * 1024)))

writerSP :: (Monad m) => (a -> m ()) -> SP m a ()
writerSP = mapSP

byLine :: (Monad m) => SP m ByteString ByteString
byLine = Get (wait empty)
  where
    wait :: ByteString -> ByteString -> SP m ByteString ByteString
    wait buf str
      | null buf  = run' (split '\n' str)
      | otherwise = run buf (split '\n' str)

    run :: ByteString -> [ByteString] -> SP m ByteString ByteString
    run buf []          = Get (wait buf)
    run  _  (rest:[])   = Get (wait rest)
    run buf (str:rest)  = Put (buf `append` str) (run' rest)

    run' :: [ByteString] -> SP m ByteString ByteString
    run' []          = Get (wait empty)
    run' (rest:[])   = Get (wait rest)
    run' (str:rest)  = Put str (run' rest)

-- * Example Code

slurp :: (MonadIO m) => Handle -> MsecTimeout -> (ByteString -> m ()) -> m ()
slurp h to f = whileM (waitForInput h to) (liftIO (hGetNonBlocking h (8 * 1024)) >>= f)

wcByteString :: (Monad m) => ByteString -> StateT WordCount m ()
wcByteString str = get >>= \st@(WC _ _ _ _) -> put $! foldl' (flip wc) st str

wcStrSP :: (Monad m) => SP m ByteString WordCount
wcStrSP = Get (run initWC)
  where
    run st@(WC _ _ _ _) buf
      | null buf  = Put st zeroArrow
      | otherwise = Get (run $! foldl' (flip wc) st buf)

bufferReader :: (MonadIO m) => Handle -> MsecTimeout -> (Ptr Word8, Int) -> SP m () (Ptr Word8, Int)
bufferReader h to (p,n) =
    readerSP' (waitForInput h to) (liftM (\i -> (p,i)) (liftIO (hGetBufNonBlocking h p n))) (Put (p,0) zeroArrow)

wcBufSP :: (MonadIO m) => SP m (Ptr Word8, Int) WordCount
wcBufSP = Get (run initWC)
  where
    run :: (MonadIO m) => WordCount -> (Ptr Word8, Int) -> SP m (Ptr Word8, Int) WordCount
    run st@(WC _ _ _ _) (_,0) = Put st zeroArrow
    run st@(WC _ _ _ _) (p,n) = Block $ liftM (Get . run) (liftIO (wcBuffer (p,n) st))

main :: IO ()
main = do
  let bufsize = 8 * 1024

  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering

  when False $ execStateT (slurp stdin wcByteString) initWC >>= print

  let getBuffer h (p,n) = liftM (\i -> (p,i)) (hGetBufNonBlocking h p n)
      moreInput h       = waitForInput h (-1)

  when False $ allocaBytes bufsize $ \ptr ->
    runSP $ bufferReader stdin (-1) (ptr,bufsize) >>> wcBufSP >>> writerSP print

  let consume buf = get >>= \st -> liftIO (wcBuffer buf st) >>= put

  when True $ allocaBytes bufsize $ \ptr -> do
    cnt <- flip execStateT initWC $
      runSP $ bufferReader stdin (-1) (ptr,bufsize) >>> writerSP consume
    print cnt

  when False $ do
    cnt <- flip execStateT initWC $ runSP (slurpSP stdin (-1) >>> writerSP wcByteString)
    print cnt

  when False $ runSP $ slurpSP stdin (-1) >>> wcStrSP >>> writerSP print
