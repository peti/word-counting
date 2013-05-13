-- Streambuf.hs

module Streambuf where

import Control.Exception
import Control.Monad.State
import Data.Char
import Foreign
import Foreign.C.Types

----- Fundamental I/O Primitives

type Byte     = Word8
type BytePtr  = Ptr Byte
type ByteSize = CSize
type Iovec    = (BytePtr, ByteSize)

toChar :: Byte -> Char
toChar = chr . fromIntegral

toByte :: Char -> Byte
toByte = fromIntegral . ord

toInt :: ByteSize -> Int
toInt = fromIntegral

toSize :: Int -> ByteSize
toSize = fromIntegral

data Iobuf = IOB !ByteSize      -- buffer size
                 !ByteSize      -- unused space at the front
                 !BytePtr       -- begin of memory buffer
                 !ByteSize      -- content length

saneIOB :: Iobuf -> Iobuf
saneIOB iob@(IOB cap gap ptr len) =
  assert (ptr /= nullPtr) $
  assert (cap > 0) $
  assert (cap >= len + gap) $
  iob

type StreamBuf = StateT Iobuf

bptr, eptr :: (Monad m) => StreamBuf m BytePtr
bptr = gets $ \(IOB _ gap p _) -> p `plusPtr` toInt gap
eptr = gets $ \(IOB _ gap p l) -> p `plusPtr` toInt (gap + l)

freeSpace, dataLen :: (Monad m) => StreamBuf m ByteSize
freeSpace = gets $ \(IOB cap gap _ len) -> cap - gap - len
dataLen   = gets $ \(IOB  _   _  _ len) -> len

flushGap :: (MonadIO m) => StreamBuf m ()
flushGap = gets saneIOB >>= f
  where
  f (IOB  _  0   _   _ ) = return ()
  f (IOB cap _  ptr len) = do
    p <- bptr
    liftIO $ copyBytes ptr p (toInt len)
    put (IOB cap 0 ptr len)

drain :: (Monad m) => ByteSize -> StreamBuf m ()
drain i = modify (f . saneIOB)
  where
  f (IOB cap gap ptr len) =
    assert (i <= len) $
    case (gap + i, len - i) of
      ( _  ,  0  ) -> IOB cap  0   ptr  0
      (gap', len') -> IOB cap gap' ptr len'

isFull, isEmpty :: (Monad m) => StreamBuf m Bool
isFull  = gets $ \(IOB cap _ _ len) -> cap == len
isEmpty = gets $ \(IOB  _  _ _ len) -> len == 0
