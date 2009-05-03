--- Copyright © 2009 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "MIT License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

module Log (LogChan, initLog, logMsg, alsoLogMsg)
where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan
import System.IO

newtype LogChan = LogChan (Chan String)

run_log :: Handle -> Chan String -> IO ()
run_log output log_chan =
    forever $ do
      msg <- readChan log_chan
      hPutStrLn output msg

logMsg :: LogChan -> String -> IO ()
logMsg (LogChan log_chan) = writeChan log_chan

alsoLogMsg :: Handle -> LogChan -> String -> IO ()
alsoLogMsg primary log_chan msg = do
    hPutStrLn primary msg
    logMsg log_chan msg

initLog :: IO LogChan
initLog = do
  log_chan <- newChan
  forkIO $ run_log stdout log_chan
  return $ LogChan log_chan
