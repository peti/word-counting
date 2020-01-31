module Main ( main ) where

import BlockIO
import WordCounting

import Foreign
import System.IO

bufsize :: ByteCount                -- our I/O buffer size
bufsize = 4096

countBuf :: BlockHandler WordCount
countBuf (Buf cap p n) st = do
  st' <- count p n st
  return ((Buf cap p 0), st')

count :: Ptr Word8 -> ByteCount -> WordCount -> IO WordCount
count  _  0 st@(WC _ _ _ _) = return st
count ptr n st@(WC _ _ _ _) = do
  c <- peek ptr
  let st' = wc (toEnum (fromEnum c)) st
  count (ptr `plusPtr` 1) (n - 1) st'

main :: IO ()
main = do
  WC _ l w c <- runLoop stdin bufsize countBuf initWC
  putStrLn . shows l . (' ':) . shows w . (' ':) $ show c
