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

import Control.Concurrent.MVar
import Control.Monad
import Data.Maybe
import Network
import System.IO

import System.Console.ParseArgs

import Log
import Service
    
data Option = OptionPort
            | OptionInit
              deriving (Ord, Eq, Show)

argd :: [ Arg Option ]
argd = [ Arg { argIndex = OptionPort,
               argName = Just "port",
               argAbbr = Just 'p',
               argData = argDataDefaulted "port" ArgtypeInt 3589,
               argDesc = "Server port" },
         Arg { argIndex = OptionInit,
               argName = Just "init",
               argAbbr = Nothing,
               argData = argDataRequired "admin-pw" ArgtypeString,
               argDesc = "Setup or upgrade server" } ]

master_init :: Int -> LogIO Socket
master_init port_num = do
  logMsg $ "listening for game on port " ++ show port_num
  liftIO $ listenOn $ PortNumber $ fromIntegral port_num

master_accept :: Socket -> LogIO (Handle, String)
master_accept listen_socket = do
  (handle, hostname, client_port) <- liftIO $ Network.accept listen_socket
  let client_id = hostname ++ ":" ++ show client_port
  logMsg $ "handling client " ++ client_id
  liftIO $ hSetBuffering handle LineBuffering
  return (handle, client_id)

run_service :: Int -> LogIO ()
run_service port = do
  state0 <- liftIO $ initServiceState
  state <- liftIO $ newMVar state0
  master <- master_init port
  forever $ do
    client <- master_accept master
    forkLogIO $ doCommands client state

main :: IO ()
main = do
  a <- parseArgsIO ArgsComplete argd
  let port = fromJust (getArgInt a OptionPort)
  case gotArg a OptionInit of
     True -> do
       let admin_pw = fromJust (getArgString a OptionInit)
       initService port admin_pw
     False ->
       withSocketsDo $ withLogDo stdout (run_service port)

