--- Copyright © 2009 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "MIT License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

module Log (LogIO, liftIO, withLogDo, logMsg, alsoLogMsg, forkLogIO, catchLogIO)
where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.Reader
import System.IO
import System.IO.Error

newtype LogChan = LogChan (Chan String)

type LogIO a = ReaderT LogChan IO a

run_log :: Handle -> LogChan -> IO ()
run_log output (LogChan log_chan) =
    forever $ do
      msg <- readChan log_chan
      hPutStrLn output msg
      hFlush output

logMsg :: String -> LogIO ()
logMsg msg = do
  LogChan log_chan <- ask
  liftIO $ writeChan log_chan msg

alsoLogMsg :: Handle -> String -> LogIO ()
alsoLogMsg primary msg = do
    liftIO $ hPutStrLn primary msg
    logMsg msg

withLogDo :: Handle -> LogIO () -> IO ()
withLogDo h actions = do
  log_chan <- newChan
  tid <- forkIO $ run_log h (LogChan log_chan)
  runReaderT actions (LogChan log_chan)
  hClose h
  killThread tid

forkLogIO :: LogIO () -> LogIO ThreadId
forkLogIO actions = do
  log_chan <- ask
  liftIO $ forkIO $ runReaderT actions log_chan

catchLogIO :: LogIO () -> (IOError -> LogIO ()) -> LogIO ()
catchLogIO actions handler = do
  log_chan <- ask
  liftIO $ catch (runReaderT actions log_chan)
                 (\e -> runReaderT (handler e) log_chan)
