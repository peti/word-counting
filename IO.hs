{-# LANGUAGE NoMonomorphismRestriction #-}

module IO where

import System.IO
import System.IO.Error
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Control.Monad
import Control.Applicative
import Control.Exception
import Control.Monad.Trans
import Control.Monad.Trans.Cont
import Control.Monad.Trans.State
import Control.Monad.IO.Peel
import Control.Monad.Loops
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TBQueue

io :: MonadIO m => IO a -> m a
io = liftIO

withFile' :: MonadIO m => FilePath -> IOMode -> (Handle -> m a) -> m ()
withFile' path mode f = io (hPutStrLn stderr ("open "++show path) >> openFile path mode) >>= \h -> f h >> io (hPutStrLn stderr ("close "++show h) >> hClose h)

hGetLine' :: MonadIO m => Handle -> m (Maybe String)
hGetLine' h = io (catchJust (guard . isEOFError) (Just <$> hGetLine h) (\_ -> return Nothing))

lineReader :: MonadIO m => Handle -> (String -> m Bool) -> m ()
lineReader h f = whileM_ (hGetLine' h >>= maybe (return False) f) (return ())

print' :: (Show a, MonadIO m) => a -> m ()
print' = io . print

main' :: IO ()
main' = do
  q1 <- newEmptyMVar
  q2 <- newEmptyMVar

  let readProcess path queue = withFile' path ReadMode (\h -> lineReader h (\l -> True <$ putMVar queue l))
  withAsync (readProcess "/etc/passwd" q1) $ \p1 ->
    withAsync (readProcess "/etc/group" q2) $ \p2 -> do
      link p1; link p2
      forever $
        (,) <$> takeMVar q1 <*> takeMVar q2 >>= print
  return ()

main :: IO ()
main = flip runContT return $ do
  withFile' "/etc/passwd" ReadMode $ \h1 -> do
    withFile' "/etc/group" ReadMode $ \h2 -> do
      lineReader h1 $ \l1 -> do
        lineReader h2 $ \l2 -> do
          print' (l1,l2)
          return False
        return True
