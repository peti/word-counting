module Main ( main ) where

import System.IO
import Foreign ( allocaBytes )

bufsize :: Int
bufsize = 4 * 1024

catBuf :: Handle -> Handle -> IO ()
catBuf hIn hOut = allocaBytes bufsize input
  where
  input ptr    = hGetBuf hIn ptr bufsize >>= output ptr
  output  _  0 = return ()
  output ptr n = hPutBuf hOut ptr n >> input ptr

main :: IO ()
main = do
  mapM_ (\h -> hSetBinaryMode h True) [ stdin, stdout ]
  mapM_ (\h -> hSetBuffering h NoBuffering) [ stdin, stdout ]
  catBuf stdin stdout
