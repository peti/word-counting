module WordCounting where

type Count = Int
data WordCount = WC !Bool !Count !Count !Count
                     deriving (Show)

initWC :: WordCount
initWC = WC True 0 0 0

wc :: Char -> WordCount -> WordCount
wc '\n' (WC _     l w c) = WC True (l+1)  w   (c+1)
wc ' '  (WC _     l w c) = WC True   l    w   (c+1)
wc '\t' (WC _     l w c) = WC True   l    w   (c+1)
wc  _   (WC True  l w c) = WC False  l  (w+1) (c+1)
wc  _   (WC False l w c) = WC False  l    w   (c+1)
