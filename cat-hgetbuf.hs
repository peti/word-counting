module Main ( main ) where

import System.IO
import System.Environment
import Foreign ( allocaBytes )

catBuf :: Int -> Handle -> Handle -> IO ()
catBuf bufsize hIn hOut = allocaBytes bufsize input
  where
  input ptr    = hGetBuf hIn ptr bufsize >>= output ptr
  output  _  0 = return ()
  output ptr n = hPutBuf hOut ptr n >> input ptr

main :: IO ()
main = do
  args <- getArgs
  let bufsize = if null args then 4096 else read (head args)
  mapM_ (`hSetBinaryMode` True) [ stdin, stdout ]
  mapM_ (`hSetBuffering` NoBuffering) [ stdin, stdout ]
  catBuf bufsize stdin stdout
