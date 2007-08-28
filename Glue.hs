module Glue
  ( module Glue
  , module System.IO
  , module System.IO.Error
  , module Control.Monad.Error
  , module Timeout
  )
  where

import System.IO
import System.IO.Error
import Control.Monad.Error
import Control.Exception ( bracket )
import Timeout

-- * Error Handling

timeoutErrorType :: IOError
timeoutErrorType = userError "timeout"

isTimeoutError :: IOError -> Bool
isTimeoutError e
  | isUserError e = ioeGetErrorString e == ioeGetErrorString timeoutErrorType
  | otherwise     = False

inputTimeout, outputTimeout :: Handle -> IOError
inputTimeout h  = annotateIOError timeoutErrorType "input" (Just h) Nothing
outputTimeout h = annotateIOError timeoutErrorType "output" (Just h) Nothing

onError :: (MonadError e m) => (e -> Bool) -> a -> m a -> m a
onError isE a f = catchError f (\e -> if isE e then return a else throwError e)

ioErrorScope :: String -> IO a -> IO a
ioErrorScope msg = modifyIOError (\e -> annotateIOError e msg Nothing Nothing)

-- * I/O Primitives

type MsecTimeout = Int

timeoutError :: IOError -> MsecTimeout -> IO a -> IO a
timeoutError e to f = timeout to f >>= maybe (ioError e) return

waitForInput :: (MonadIO m) => Handle -> MsecTimeout -> m Bool
waitForInput h to = liftIO $
                      timeoutError (inputTimeout h) to $
                        onError isEOFError False $
                          hWaitForInput h (-1)

withBuffering :: Handle -> BufferMode  -> IO a -> IO a
withBuffering h m f = bracket (hGetBuffering h) (hSetBuffering h)
                              (\_ -> hSetBuffering h m >> f)

withoutBuffering :: Handle -> IO a -> IO a
withoutBuffering h = withBuffering h NoBuffering

-- * Monadic Flow Control

ifM :: (Monad m) => m Bool -> (m a, m a) -> m a
ifM cond (true,false) = cond >>= \b -> if b then true else false

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM cond f = ifM cond (f, return ())

whileM :: (Monad m) => m Bool -> m () -> m ()
whileM cond f = whenM cond (f >> whileM cond f)
