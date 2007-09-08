module Main ( main ) where

import System.IO
import qualified Data.ByteString.Base as Str
import qualified Data.ByteString as Str
import Data.ByteString ( ByteString )

bufsize :: Int
bufsize = 4 * 1024

hGet :: Handle -> ByteString -> IO ByteString
hGet h buf = do i <- Str.unsafeUseAsCStringLen buf (\(p,n) -> hGetBuf h p n)
                return (Str.unsafeTake i buf)

catString :: Handle -> Handle -> IO ()
catString hIn hOut = Str.create bufsize (\_ -> return ()) >>= input
  where
  input buf      = hGet hIn buf >>= output buf
  output buf b
    | Str.null b = return ()
    | otherwise  = Str.hPut hOut b >> input buf

main :: IO ()
main = do
  mapM_ (\h -> hSetBuffering h NoBuffering) [ stdin, stdout ]
  catString stdin stdout
