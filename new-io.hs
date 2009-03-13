{-# LANGUAGE Arrows #-}
{-# OPTIONS -fglasgow-exts #-}

module Main ( main ) where

import Prelude hiding ( null )
import System.Environment ( getArgs )
import Control.Arrow.SP
import Control.Monad.State
import Data.ByteString.Char8
import Data.Monoid
import Foreign
import Data.Char
import WordCounting
import Glue

-- * Memory Buffers

type ByteSize   = Int
type BytePtr    = Ptr Word8
type Buffer     = (BytePtr, ByteSize)
type BufferSize = ByteSize

withBuffer :: String -> (Buffer -> IO a) -> IO a
withBuffer buf f = withArray (Prelude.map (fromIntegral . ord) buf) (\p -> f (p, Prelude.length buf))

-- * Stream Processig Arrow

readerSP' :: (Monad m) => m Bool -> m a -> SP m () a -> SP m () a
readerSP' waiter reader eof = Block $
  ifM waiter (liftM (\a -> Put a (readerSP' waiter reader eof)) reader, return eof)

readerSP :: (Monad m, Monoid a) => m Bool -> m a -> SP m () a
readerSP waiter reader = readerSP' waiter reader (Put mempty zeroArrow)

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

-- * SPIO

type SPIO i o = forall m. MonadIO m => SP m i o

slurpSP :: Handle -> MsecTimeout -> SPIO () ByteString
slurpSP h to = readerSP (waitForInput h to) (liftIO (hGetNonBlocking h (8 * 1024)))

bufferReader :: Handle -> MsecTimeout -> Buffer -> SPIO () Buffer
bufferReader h to (p,n) =
    readerSP' (waitForInput h to) (liftM (\i -> (p,i)) (liftIO (hGetBufNonBlocking h p n))) (Put (p,0) zeroArrow)

bufferWriter :: Handle -> MsecTimeout -> SPIO Buffer ()
bufferWriter h to = mapSP $ \(p,n) -> liftIO (timeoutError (outputTimeout h) to (hPutBuf h p n))

-- * Word-counting Example Code

wcByteString :: (Monad m) => ByteString -> StateT WordCount m ()
wcByteString str = get >>= \st@(WC _ _ _ _) -> put $! foldl' (flip wc) st str

wcStrSP :: (Monad m) => SP m ByteString WordCount
wcStrSP = Get (run initWC)
  where
    run st@(WC _ _ _ _) buf
      | null buf  = Put st zeroArrow
      | otherwise = Get (run $! foldl' (flip wc) st buf)

wcBufSP :: SPIO Buffer WordCount
wcBufSP = Get (run initWC)
  where
    run :: (MonadIO m) => WordCount -> Buffer -> SP m Buffer WordCount
    run st@(WC _ _ _ _) (_,0) = Put st zeroArrow
    run st@(WC _ _ _ _) (p,n) = Block $ liftM (Get . run) (liftIO (wcBuffer (p,n) st))

-- * HTTP Daemon

httpd :: BufferSize -> (Handle,MsecTimeout) -> (Handle,MsecTimeout) -> IO ()
httpd bufsize (hIn,toIn) (hOut,toOut) = do
  modifyIOError (\e -> annotateIOError e "httpd" Nothing Nothing) $
    withoutBuffering hIn $
      withoutBuffering hOut $
        allocaBytes bufsize $ \ptr ->
          runErrorT $
            runSP $
              let reader         =  bufferReader hIn toIn (ptr,bufsize)
                  writer         =  bufferWriter hOut toOut
                  throwShutdown  =  mapSP (\buf -> if nullBuf buf then throwError "" else return buf)
                  nullBuf (_,0)  =  True
                  nullBuf (p,_)  =  p == nullPtr
              in
              reader >>> httpdSP >>> throwShutdown >>> writer
  return ()

httpdSP :: SPIO Buffer Buffer
httpdSP = mapSP (\_ -> return (nullPtr,0))

type Uri = ByteString

data RequestMethod = GET | POST
                   deriving (Show)

data HttpEvent = Request RequestMethod Uri (Int,Int)
               | HostHeader ByteString
               | ConnectionHeader ByteString
               | UnknownHeader ByteString ByteString
               | EndOfHeader
               deriving (Show)

-- * Command-line driver

main :: IO ()
main = do
  let bufsize :: Int
      bufsize = 8 * 1024

  hSetBuffering stdin NoBuffering
  hSetBuffering stdout (BlockBuffering (Just bufsize))
  hSetBuffering stderr LineBuffering

  let consume :: Buffer -> StateT WordCount IO ()
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
