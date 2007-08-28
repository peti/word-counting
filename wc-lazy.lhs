\ignore{ %%%%% Declarations required for Haskell %%%%%%%%%%%%%%%%%%%%%
\begin{code}
module Main ( main ) where

import IO
\end{code}
}

\begin{code}
main :: IO ()
main = do
  file <- hGetContents stdin
  putStrLn $ show (length (lines file)) ++ " " ++
             show (length (words file)) ++ " " ++
             show (length file)
\end{code}

%%%%% configure emacs
%
% Local variables:
% mode: latex
% fill-column: 72
% mmm-classes: literate-haskell-latex
% End:
