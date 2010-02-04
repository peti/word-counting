\ignore{ %%%%% Declarations required for Haskell %%%%%%%%%%%%%%%%%%%%%
\begin{code}
module Main ( main ) where

import Control.Monad.State
import Data.ByteString.Char8
import WordCounting
import Glue
\end{code}
}

\begin{code}
slurp :: (MonadIO m) => Handle -> MsecTimeout -> (ByteString -> m ()) -> m ()
slurp h to f = whileM (waitForInput h to) (liftIO (hGetNonBlocking h (8 * 1024)) >>= f)

wcByteString :: (Monad m) => ByteString -> StateT WordCount m ()
wcByteString str = get >>= \st@(WC _ _ _ _) -> put $! foldl' (flip wc) st str

main :: IO ()
main = do
  let bufsize :: Int
      bufsize = 8 * 1024

  hSetBinaryMode stdin True
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout (BlockBuffering (Just bufsize))
  hSetBuffering stderr LineBuffering

  execStateT (slurp stdin (-1) wcByteString) initWC >>= print
\end{code}

\begin{verbatim} aucl s

%%%%% configure emacs
%
% Local variables:
% mode: latex
% fill-column: 72
% mmm-classes: literate-haskell-latex
% End:
