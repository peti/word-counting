module WordCounting ( WordCount(..), initWC, wc, wcBuffer ) where

import Foreign
import Data.Char

data WordCount = WC !Bool !Int !Int !Int
                     deriving (Show)

initWC :: WordCount
initWC = WC True 0 0 0

wc :: Char -> WordCount -> WordCount
wc '\n' (WC _     l w c) = WC True (l+1)  w   (c+1)
wc ' '  (WC _     l w c) = WC True   l    w   (c+1)
wc '\t' (WC _     l w c) = WC True   l    w   (c+1)
wc  _   (WC True  l w c) = WC False  l  (w+1) (c+1)
wc  _   (WC False l w c) = WC False  l    w   (c+1)

wcBuffer :: (Ptr Word8, Int) -> WordCount -> IO WordCount
wcBuffer (_, 0) st@(WC _ _ _ _)  = return st
wcBuffer (p, n) st@(WC _ _ _ _)  = do
  c <- fmap (chr . fromIntegral) (peek p)
  wcBuffer (p `plusPtr` 1, n - 1) (wc c st)
