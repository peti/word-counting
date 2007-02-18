module Async
  (
        Async                   -- Async a = Child ThreadId (AsyncMVar a)
  ,     forkAsync               -- :: IO a -> IO (Async a)
  ,     throwToAsync            -- :: Async a -> Exception -> IO ()
  ,     killAsync               -- :: Async a -> IO ()
  ,     isReadyAsync            -- :: Async a -> IO Bool
  ,     waitForAsync            -- :: Async a -> IO a

  ,     parIO                   -- :: IO a -> IO a -> IO a
  )
  where

import Control.Concurrent
import Control.Exception

type AsyncMVar a = MVar (Either Exception a)

data Async a = Child ThreadId (AsyncMVar a)

forkAsync' :: IO a -> AsyncMVar a -> IO (Async a)
forkAsync' f mv = fmap (\p -> Child p mv) (forkIO f')
  where
    f' = block (try f >>= tryPutMVar mv >> return ())

forkAsync :: IO a -> IO (Async a)
forkAsync f = newEmptyMVar >>= forkAsync' f

throwToAsync :: Async a -> Exception -> IO ()
throwToAsync (Child pid _) = throwTo pid

killAsync :: Async a -> IO ()
killAsync (Child pid _) = killThread pid

isReadyAsync :: Async a -> IO Bool
isReadyAsync (Child _ mv) = fmap not (isEmptyMVar mv)

waitForAsync :: Async a -> IO a
waitForAsync (Child _ sync) = fmap (either throw id) (readMVar sync)

-- Run both computations in parallel and return the @a@ value
-- of the computation that terminates first. An exception in
-- either of the two computations aborts the entire parIO
-- computation.

parIO :: IO a -> IO a -> IO a
parIO f g = do
  sync <- newEmptyMVar
  bracket
    (forkAsync' f sync)
    (killAsync)
    (\_ -> bracket
             (forkAsync' g sync)
             (killAsync)
             (waitForAsync))
