module Main ( main ) where

import Glue
import WordCounting

import Control.Monad.State
import Data.ByteString.Char8

slurp :: (MonadIO m) => Handle -> MsecTimeout -> (ByteString -> m ()) -> m ()
slurp h to f = whileM (waitForInput h to) (liftIO (hGetNonBlocking h (8 * 1024)) >>= f)

wcByteString :: (Monad m) => ByteString -> StateT WordCount m ()
wcByteString str = get >>= \st@(WC _ _ _ _) -> put $! foldl' (flip wc) st str

main :: IO ()
main = do
  let bufsize :: Int
      bufsize = 8 * 1024

  hSetBinaryMode stdin True
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout (BlockBuffering (Just bufsize))
  hSetBuffering stderr LineBuffering

  execStateT (slurp stdin (-1) wcByteString) initWC >>= print
