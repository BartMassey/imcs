{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}
--- Copyright © 2009 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "MIT License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

module Log (LogIO, MonadLogIO, liftIO, withLogDo, logMsg, alsoLogMsg,
           forkLogIO, catchLogIO, whileLogIO, sPutStrLn)
where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception.Base
import Control.Monad.Error
import Control.Monad.Reader
import Data.IORef
import System.IO

import SNewLine

newtype LogMsg = LogMsg String

newtype LogChan = LogChan (Chan LogMsg)

type LogIO = ReaderT LogChan IO

class (MonadIO m, MonadReader LogChan m) => MonadLogIO m
instance MonadLogIO LogIO
instance (Error e, MonadLogIO m) => MonadLogIO (ErrorT e m)

run_log :: Handle -> LogChan -> IO ()
run_log output (LogChan log_chan) =
    forever $ do
      LogMsg str <- readChan log_chan
      hPutStrLn output str
      hFlush output

logMsg :: MonadLogIO m => String -> m ()
logMsg msg = do
  LogChan log_chan <- ask
  liftIO $ writeChan log_chan (LogMsg msg)

alsoLogMsg :: MonadLogIO m => Handle -> String -> m ()
alsoLogMsg primary msg = do
    sPutStrLn primary msg
    logMsg msg

withLogDo :: Handle -> LogIO a -> IO a
withLogDo h actions = do
  log_chan <- newChan
  tid <- forkIO $ run_log h (LogChan log_chan)
  r <- runReaderT actions (LogChan log_chan)
  hClose h
  killThread tid
  return r

forkLogIO :: LogIO () -> LogIO ThreadId
forkLogIO actions = do
  log_chan <- ask
  liftIO $ forkIO $ runReaderT actions log_chan

catchLogIO :: LogIO () -> (IOError -> LogIO ()) -> LogIO ()
catchLogIO actions handler = do
  log_chan <- ask
  liftIO $ catch (runReaderT actions log_chan)
                 (\e -> runReaderT (handler e) log_chan)

whileLogIO :: IORef Bool -> LogIO () -> LogIO ()
whileLogIO b a = do
  cond <- liftIO $ readIORef b
  case cond of
    False -> return ()
    True -> a >> whileLogIO b a

sPutStrLn :: MonadLogIO m => Handle -> String -> m ()
sPutStrLn h s = liftIO $ sPutStrNL h s
