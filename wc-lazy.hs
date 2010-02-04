module Main ( main ) where

import System.IO

main :: IO ()
main = do
  hSetBinaryMode stdin True
  file <- hGetContents stdin
  putStrLn $ show (length (lines file)) ++ " " ++
             show (length (words file)) ++ " " ++
             show (length file)
