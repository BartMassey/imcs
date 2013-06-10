{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances, CPP #-}
--- Copyright © 2009 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "MIT License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

module Log (LogIO, MonadLogIO, liftIO, withLogDo, logMsg, alsoLogMsg,
           forkLogIO, catchLogIO, sPutStrLn)
where

#if __GLASGOW_HASKELL__ < 706
import Prelude hiding (catch)
#endif
import Control.Concurrent
import Control.Exception.Base
import Control.Monad.Error
import Control.Monad.Reader
import System.IO

import SNewLine

type LogIO = ReaderT Handle IO

class (MonadIO m, MonadReader Handle m) => MonadLogIO m
instance MonadLogIO LogIO
instance (Error e, MonadLogIO m) => MonadLogIO (ErrorT e m)

logMsg :: MonadLogIO m => String -> m ()
logMsg msg = do
  h <- ask
  liftIO $ do
    hPutStrLn h msg
    hFlush h

alsoLogMsg :: MonadLogIO m => Handle -> String -> m ()
alsoLogMsg primary msg = do
    sPutStrLn primary msg
    logMsg msg

withLogDo :: Handle -> LogIO a -> IO a
withLogDo h actions = do
  r <- runReaderT actions h
  hClose h
  return r

forkLogIO :: LogIO () -> LogIO ThreadId
forkLogIO actions = do
  h <- ask
  liftIO $ forkIO $ runReaderT actions h

catchLogIO :: LogIO () -> (IOError -> LogIO ()) -> LogIO ()
catchLogIO actions handler = do
  h <- ask
  liftIO $ catch (runReaderT actions h)
                 (\e -> runReaderT (handler e) h)

sPutStrLn :: MonadLogIO m => Handle -> String -> m ()
sPutStrLn h s = liftIO $ sPutStrNL h s
