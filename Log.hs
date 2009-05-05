--- Copyright © 2009 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "MIT License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

module Log (LogIO, liftIO, withLogDo, logMsg, alsoLogMsg, forkLogIO, catchLogIO)
where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.Chan
import System.IO

type LogIO a = ReaderT (Chan String) IO a

newtype LogChan = LogChan (Chan String)

run_log :: Handle -> Chan String -> IO ()
run_log output log_chan =
    forever $ do
      msg <- readChan log_chan
      hPutStrLn output msg

logMsg :: String -> LogIO ()
logMsg msg = do
  log_chan <- ask
  liftIO $ writeChan log_chan msg

alsoLogMsg :: Handle -> String -> LogIO ()
alsoLogMsg primary msg = do
    liftIO $ hPutStrLn primary msg
    logMsg msg

withLogDo :: Handle -> LogIO () -> IO ()
withLogDo handle actions = do
  log_chan <- newChan
  forkIO $ run_log handle log_chan
  runReaderT actions log_chan

forkLogIO :: LogIO () -> LogIO ThreadId
forkLogIO actions = do
  log_chan <- ask
  liftIO $ forkIO $ runReaderT actions log_chan

catchLogIO :: Exception e => LogIO () -> (e -> LogIO ()) -> LogIO ()
catchLogIO actions handler = do
  log_chan <- ask
  liftIO $ catch (runReaderT actions log_chan)
                 (\e -> runReaderT (handler e) log_chan)
