{-
  An implementation of the 'wc' benchmark.
  http://shootout.alioth.debian.org/

  Written by Peter Simons <simons@cryp.to>.
  This code is in the public domain.

  Compile with: ghc -O2 -funbox-strict-fields -o wc wc.hs
-}

module Main ( main ) where

import WordCounting

import Foreign
import System.IO

bufsize :: Int                  -- our I/O buffer size
bufsize = 4096

type IOHandler st = Ptr Word8 -> Int -> st -> IO st

countBuf :: IOHandler WordCount
countBuf  _  0 st@(WC _ _ _ _) = return st
countBuf ptr n st@(WC _ _ _ _) = do
  c <- peek ptr
  let st' = wc (toEnum (fromEnum c)) st
  countBuf (ptr `plusPtr` 1) (n - 1) st'

loop :: Handle -> Int -> IOHandler st -> st -> IO st
loop h n f st' = allocaArray n (\ptr' -> loop' ptr' st')
  where
  loop' ptr st = st `seq` do
    rc <- hGetBuf h ptr n
    if rc == 0
       then return st
       else f ptr rc st >>= loop' ptr

main :: IO ()
main = do
  WC _ l w c <- loop stdin bufsize countBuf initWC
  putStrLn . shows l . (' ':) . shows w . (' ':) $ show c
