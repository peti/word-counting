{-# OPTIONS -fglasgow-exts -farrows #-}

module Main ( main ) where

import Prelude hiding ( null )
import System.Environment ( getArgs )
import Control.Arrow.SP
import Control.Monad.State
import Data.ByteString.Char8
import Data.Monoid
import Foreign
import WordCounting
import Glue

-- * Stream Processig Arrow

readerSP' :: (Monad m) => m Bool -> m a -> SP m () a -> SP m () a
readerSP' waiter reader eof = Block $
  ifM waiter (liftM (\a -> Put a (readerSP' waiter reader eof)) reader, return eof)

readerSP :: (Monad m, Monoid a) => m Bool -> m a -> SP m () a
readerSP waiter reader = readerSP' waiter reader (Put mempty zeroArrow)

slurpSP :: (MonadIO m) => Handle -> MsecTimeout -> SP m () ByteString
slurpSP h to = readerSP (waitForInput h to) (liftIO (hGetNonBlocking h (8 * 1024)))

writerSP :: (Monad m) => (a -> m ()) -> SP m a ()
writerSP = mapSP

byLine :: (Monad m) => SP m ByteString ByteString
byLine = Get (wait empty)
  where
    wait :: ByteString -> ByteString -> SP m ByteString ByteString
    wait buf str
      | null buf  = run' (split '\n' str)
      | otherwise = run buf (split '\n' str)

    run :: ByteString -> [ByteString] -> SP m ByteString ByteString
    run buf []          = Get (wait buf)
    run  _  (rest:[])   = Get (wait rest)
    run buf (str:rest)  = Put (buf `append` str) (run' rest)

    run' :: [ByteString] -> SP m ByteString ByteString
    run' []          = Get (wait empty)
    run' (rest:[])   = Get (wait rest)
    run' (str:rest)  = Put str (run' rest)

-- * Word-counting Example Code

wcByteString :: (Monad m) => ByteString -> StateT WordCount m ()
wcByteString str = get >>= \st@(WC _ _ _ _) -> put $! foldl' (flip wc) st str

wcStrSP :: (Monad m) => SP m ByteString WordCount
wcStrSP = Get (run initWC)
  where
    run st@(WC _ _ _ _) buf
      | null buf  = Put st zeroArrow
      | otherwise = Get (run $! foldl' (flip wc) st buf)

bufferReader :: (MonadIO m) => Handle -> MsecTimeout -> (Ptr Word8, Int) -> SP m () (Ptr Word8, Int)
bufferReader h to (p,n) =
    readerSP' (waitForInput h to) (liftM (\i -> (p,i)) (liftIO (hGetBufNonBlocking h p n))) (Put (p,0) zeroArrow)

wcBufSP :: (MonadIO m) => SP m (Ptr Word8, Int) WordCount
wcBufSP = Get (run initWC)
  where
    run :: (MonadIO m) => WordCount -> (Ptr Word8, Int) -> SP m (Ptr Word8, Int) WordCount
    run st@(WC _ _ _ _) (_,0) = Put st zeroArrow
    run st@(WC _ _ _ _) (p,n) = Block $ liftM (Get . run) (liftIO (wcBuffer (p,n) st))

-- * HTTP Daemon

-- * Command-line driver

main :: IO ()
main = do
  let bufsize :: Int
      bufsize = 8 * 1024

  hSetBuffering stdin NoBuffering
  hSetBuffering stdout (BlockBuffering (Just bufsize))
  hSetBuffering stderr LineBuffering

  let consume :: (Ptr Word8, Int) -> StateT WordCount IO ()
      consume buf = get >>= \st -> liftIO (wcBuffer buf st) >>= put

  allocaBytes bufsize $ \ptr -> do
  args <- getArgs
  case args of
    "wcBuffer"    : _ -> runSP $ bufferReader stdin (-1) (ptr,bufsize) >>> wcBufSP >>> writerSP print

    "wcBufferST"  : _ -> do cnt <- flip execStateT initWC $ runSP $ bufferReader stdin (-1) (ptr,bufsize) >>> writerSP consume
                            print cnt

    "wcSlurpSP"   : _ -> do cnt <- flip execStateT initWC $ runSP (slurpSP stdin (-1) >>> writerSP wcByteString)
                            print cnt

    "wcByteStrSP" : _ -> do runSP $ slurpSP stdin (-1) >>> wcStrSP >>> writerSP print

    _                 -> fail "usage: ./new-io (wcBuffer | wcBufferST | wcSlurpSP | wcByteStrSP)"
