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

import Control.Concurrent
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
            | OptionUpgrade
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
               argData = Nothing,
               argDesc = "Setup server" },
         Arg { argIndex = OptionUpgrade,
               argName = Just "upgrade",
               argAbbr = Nothing,
               argData = argDataOptional "admin-pw" ArgtypeString,
               argDesc = "Upgrade server" } ]

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
  mainThread <- liftIO $ myThreadId
  state0 <- liftIO $ initServiceState
  state <- liftIO $ newMVar state0
  master <- master_init port
  reaccept <- liftIO $ newMVar True
  forever $ do
    client <- master_accept master
    ok <- liftIO $ readMVar reaccept
    case ok of
      True -> do
        forkLogIO $ doCommands (mainThread, reaccept) client state
        return ()
      False -> do
        let (h, cid) = client
        liftIO $ hPutStrLn h $ "503 server shutdown, come back later"
        liftIO $ hClose h
        logMsg $ "refused client " ++ cid

main :: IO ()
main = do
  a <- parseArgsIO ArgsComplete argd
  let port = fromJust (getArgInt a OptionPort)
  case (gotArg a OptionInit, getArgString a OptionUpgrade) of
     (True, Just _) -> usageError a "cannot both init and upgrade"
     (False, Just admin_pw) -> upgradeService port admin_pw
     (True, Nothing) -> initService port
     (False, Nothing) ->
       withSocketsDo $ withLogDo stdout (run_service port)
