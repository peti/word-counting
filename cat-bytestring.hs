module Main ( main ) where

import System.IO
import System.Environment
import qualified Data.ByteString.Unsafe as Str
import qualified Data.ByteString.Internal as Str
import qualified Data.ByteString as Str
import Data.ByteString ( ByteString )

hGet :: Handle -> ByteString -> IO ByteString
hGet h buf = do i <- Str.unsafeUseAsCStringLen buf (uncurry (hGetBuf h))
                return (Str.unsafeTake i buf)

catString :: Int -> Handle -> Handle -> IO ()
catString bufsize hIn hOut = Str.create bufsize (\_ -> return ()) >>= input
  where
  input buf      = hGet hIn buf >>= output buf
  output buf b
    | Str.null b = return ()
    | otherwise  = Str.hPut hOut b >> input buf

main :: IO ()
main = do
  args <- getArgs
  let bufsize = if null args then 4096 else read (head args)
  mapM_ (`hSetBinaryMode` True) [ stdin, stdout ]
  mapM_ (`hSetBuffering` NoBuffering) [ stdin, stdout ]
  catString bufsize stdin stdout
