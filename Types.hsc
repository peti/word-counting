{-# OPTIONS -fffi -fglasgow-exts #-}

module Types where

import Control.Exception ( assert )
import Foreign

#include <sys/types.h>
#include <sys/socket.h>
#include <errno.h>

type Byte               = Word8
type BytePtr            = Ptr Word8
type ByteSize           = #{type size_t}

toInt                   :: ByteSize -> Int
toInt                   = fromIntegral

toSize                  :: Int -> ByteSize
toSize n                = fromIntegral (assert (n > 0) n)

data Iovec              = Iovec !BytePtr !ByteSize
                          deriving (Show, Eq)

type SockAddrSize       = #{type socklen_t}
data Endpoint           = Endpoint !(Ptr ()) !SockAddrSize

data Iomsg              = Iomsg

instance Storable Iovec where
  sizeOf _              = #{size struct iovec}
  alignment _           = alignment (nullPtr :: BytePtr)
  peek ptr              = do p <- #{peek struct iovec, iov_base} ptr
                             n <- #{peek struct iovec, iov_len} ptr
                             return (Iovec p n)
  poke ptr (Iovec p n)  = do #{poke struct iovec, iov_base} ptr p
                             #{poke struct iovec, iov_len}  ptr n

-- foreign import ccall unsafe
--   adns_synchronous :: AdnsState -> CString -> CInt -> CInt -> Ptr (Ptr Answer)
--
--   -> IO CInt
--
-- foreign import ccall unsafe "__hscore_PrelHandle_read"
--    read_rawBuffer :: FD -> RawBuffer -> Int -> CInt -> IO CInt
