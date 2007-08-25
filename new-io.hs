module Main ( main ) where

import Prelude hiding ( null )
import System.IO hiding ( hPutStrLn )
import System.IO.Error
import Control.Exception
import Control.Monad.State
import Data.ByteString.Char8
import Timeout
import WordCounting
import Control.Arrow.SP

-- * I/O Primitives

type MaxReadSize = Int
type MsecTimeout = Int

recv :: (MonadIO m) => Handle -> MaxReadSize -> m ByteString
recv h n = liftIO $ catchJust (\e -> ioErrors e >>= guard . isEOFError)
                              (tryRead True >>= retryWhenEmpty)
                              (\_ -> return empty)
    where
      tryRead           :: Bool -> IO ByteString
      tryRead False     = return empty
      tryRead True      = hGetNonBlocking h n

      retryWhenEmpty    :: ByteString -> IO ByteString
      retryWhenEmpty buf
        | null buf      = hWaitForInput h (-1) >>= tryRead
        | otherwise     = return buf

send :: (MonadIO m) => Handle -> ByteString -> m ()
send h str | null str  = liftIO (hFlush h)
           | otherwise = liftIO (hPut h str)

-- ** I/O Timeouts

mkTimeout, readTimeout, writeTimeout :: Handle -> IOError
mkTimeout      = ioeSetHandle (userError "timeout")
readTimeout  h = annotateIOError (mkTimeout h) "input"  Nothing Nothing
writeTimeout h = annotateIOError (mkTimeout h) "output" Nothing Nothing

isTimeoutError :: IOError -> Bool
isTimeoutError e = isUserError e && ioeGetErrorString e == "timeout"

recvTO :: (MonadIO m) => Handle -> MaxReadSize -> MsecTimeout -> m ByteString
recvTO h n to = liftIO $
 timeout to (recv h n) >>= maybe (ioError (readTimeout h)) return

sendTO :: (MonadIO m) => Handle -> Int -> ByteString -> m ()
sendTO h to str = liftIO $
 timeout to (send h str) >>= maybe (ioError (writeTimeout h)) return

-- * Monadic Stream Consumer

slurp :: (Monad m) => m ByteString -> (ByteString -> m ()) -> m ()
slurp source consumer =
  source >>= \a -> when (not (null a)) (consumer a >> slurp source consumer)

wcByteString :: (Monad m) => ByteString -> StateT WordCount m ()
wcByteString str = get >>= \st@(WC _ _ _ _) -> put $! foldl' (flip wc) st str

-- * Stream Processig Arrow

readerSP :: (Monad m) => m ByteString -> SP m () ByteString
readerSP source = Block $ do
  buf <- source
  return $ Put buf $ case null buf of True  -> zeroArrow
                                      False -> readerSP source

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

wcSP :: (Monad m) => SP m ByteString WordCount
wcSP = Get (run initWC)
  where
    run st@(WC _ _ _ _) buf
      | null buf  = Put st wcSP
      | otherwise = Get (run $! foldl' (flip wc) st buf)

main :: IO ()
main = do
  -- execStateT (slurp (recvTO stdin (8 * 1024) (5 * 1000000)) wcByteString) initWC >>= print

  runSP $ (readerSP (recv stdin (8 * 1024)) >>> (wcSP <+> wcSP) >>> writerSP print)
