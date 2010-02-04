module Main ( main ) where

import IO

main :: IO ()
main = do
  file <- hGetContents stdin
  putStrLn $ show (length (lines file)) ++ " " ++
             show (length (words file)) ++ " " ++
             show (length file)
