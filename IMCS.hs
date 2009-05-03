--- Copyright © 2009 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "MIT License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

--- Internet MiniChess Server
---   Play MiniChess with other people or programs
---   over the Internet.

--- Thanks to Marc Gallagher
---   http://www.ternarysoftware.com/blogs/2009/02/21/
---     playing-with-haskell-http-server/
--- for his nice service example.

import Data.Maybe
import System.IO
import Control.Monad
import Network
import Control.Concurrent
import Control.Concurrent.MVar

import System.Console.ParseArgs

import Log
import Service
    
data Option = OptionPort
              deriving (Ord, Eq, Show)

argd :: [ Arg Option ]
argd = [ Arg { argIndex = OptionPort,
               argName = Just "port",
               argAbbr = Just 'p',
               argData = argDataDefaulted "port" ArgtypeInt 3589,
               argDesc = "Server port" } ]

master_init :: Int -> LogIO Socket
master_init port_num = do
  logMsg $ "listening for game on port " ++ show port_num
  liftIO $ listenOn $ PortNumber $ fromIntegral port_num

master_accept :: Socket -> LogIO Handle
master_accept listen_socket = do
  (handle, hostname, client_port) <- liftIO $ Network.accept listen_socket
  logMsg $ "handling client " ++ hostname ++ ":" ++ show client_port
  liftIO $ hSetBuffering handle LineBuffering
  return handle

run_service :: Int -> LogIO ()
run_service port = do
  state <- liftIO $ newMVar []
  master <- master_init port
  forkLogIO $ forever $ do
    client <- master_accept master
    doCommands client state
    liftIO $ hClose client

main :: IO ()
main = do
  args <- parseArgsIO ArgsComplete argd
  let port = fromJust (getArgInt args OptionPort)
  withSocketsDo $ withLogDo stdout (run_service port)
