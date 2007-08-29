module Main ( main ) where

import System.IO
import qualified Data.ByteString as Str

bufsize :: Int
bufsize = 4 * 1024

catString :: Handle -> Handle -> IO ()
catString hIn hOut = Str.hGet hIn bufsize >>= loop
  where
  loop buf | Str.null buf = return ()
           | otherwise    = Str.hPut hOut buf >> catString hIn hOut

main :: IO ()
main = do
  mapM_ (\h -> hSetBuffering h NoBuffering) [ stdin, stdout ]
  catString stdin stdout
