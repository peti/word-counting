
module Main ( main ) where

import Prelude hiding ( null )
import System.IO
import System.IO.Error
import Control.Exception
import Control.Monad
import Control.Monad.State
import Data.ByteString.Char8
import Timeout
import WordCounting

recv :: Handle -> IO ByteString
recv h = catchJust (\e -> ioErrors e >>= guard . isEOFError)
                   (tryRead True >>= retryWhenEmpty)
                   (\_ -> return empty)
    where
      tryRead           :: Bool -> IO ByteString
      tryRead False     = return empty
      tryRead True      = hGetNonBlocking h 4096

      retryWhenEmpty    :: ByteString -> IO ByteString
      retryWhenEmpty buf
        | null buf      = hWaitForInput h (-1) >>= tryRead
        | otherwise     = return buf

recvTO :: Handle -> Int -> IO ByteString
recvTO h to = liftM (maybe empty id) (timeout to (recv h))

slurp :: (Monad m) => m ByteString -> (ByteString -> m ()) -> m ()
slurp source consume = do
  a <- source
  when (not (null a)) (consume a >> slurp source consume)

wcByteString str = get >>= \st -> put (foldl' (flip wc) st str)

main :: IO ()
main = do
  count <- execStateT (slurp (liftIO (recv stdin)) wcByteString) initWC
  print count
