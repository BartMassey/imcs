--- Copyright © 2009 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "MIT License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

module Service(ServiceState, initServiceState, doCommands) where

import Prelude hiding (catch)

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad
import Data.Char
import Data.IORef
import System.IO
import System.Time

import Version
import Log
import Game

log_path :: String
log_path = "log"

game_id_path :: String
game_id_path = log_path ++ "/GAMEID"

default_time :: Maybe Int
default_time = Just 300000

data Wakeup = Wakeup String String Handle
data GamePost = GameResv Int String String Char (Chan Wakeup)
data ServiceState = ServiceState Int [GamePost]

write_game_id :: Int -> IO ()
write_game_id game_id = do
  idh <- openFile game_id_path WriteMode
  hPrint idh game_id
  hClose idh

initServiceState :: IO ServiceState
initServiceState = do
  next_game_id <- catch do_game_id cleanup
  return $ ServiceState next_game_id []
  where
    do_game_id = do
      idh <- openFile game_id_path ReadMode
      l <- hGetLine idh
      hClose idh
      return $ read l :: IO Int
    cleanup :: IOException -> IO Int
    cleanup e = do
      write_game_id 1
      return 1

parse_int :: String -> Maybe Int
parse_int "" = Nothing
parse_int s
    | all isDigit s = Just (go 0 s)
    | otherwise = Nothing
    where
      go cur [] = cur
      go cur (c : cs) = go (10 * cur + digitToInt c) cs

while :: IORef Bool -> LogIO () -> LogIO ()
while b a = do
  cond <- liftIO $ readIORef b
  case cond of
    False -> return ()
    True -> a >> while b a

doCommands :: (Handle, String) -> MVar ServiceState -> LogIO ()
doCommands (h, client_id) state = do
  liftIO $ hPutStrLn h $ "imcs " ++ version
  continue <- liftIO $ newIORef True
  me <- liftIO $ newIORef client_id
  while continue $ do
    line <- liftIO $ hGetLine h
    let command = words line
    case command of
      [] -> return ()
      ["help"] ->
        liftIO $ hPutStr h $ unlines [
          " imcs " ++ version,
          " help: this help",
          " quit: quit imcs",
          " me <name>: set my alias (default hostname:port)",
          " list: list available games (<id> <opponent-name> <opponent-color>)",
          " offer <color>: offer a game as W or B",
          " accept <id>: accept a game with an opponent" ]
      ["quit"] -> do
        logMsg $ "client " ++ client_id ++ " quits"
        liftIO $ do
          writeIORef continue False
          hClose h
      ["me", name] -> do
        logMsg $ "client " ++ client_id ++ " aka " ++ name
        liftIO $ writeIORef me name
      ["offer", color] ->
        case valid_color color of
          True -> do
            logMsg $ "client " ++ client_id ++ " offers game as " ++ color
            wakeup <- liftIO $ newChan
            liftIO $ writeIORef continue False
            ServiceState game_id game_list <- liftIO $ takeMVar state
            my_name <- liftIO $ readIORef me
            let new_game =
                    GameResv game_id my_name client_id (head color) wakeup
            let service_state' =
                    ServiceState (game_id + 1) (game_list ++ [new_game])
            liftIO $ write_game_id (game_id + 1)
            liftIO $ putMVar state service_state'
            Wakeup other_name other_id other_h
                <- liftIO $ readChan wakeup
            let run_game = do
                logMsg $ "game " ++ client_id ++
                         " (" ++ color ++ ") vs " ++
                         other_id ++ " begins"
                let (p1, p2) = case head color of
                                 'W' -> ((h, default_time),
                                         (other_h, default_time))
                                 'B' -> ((other_h, default_time),
                                         (h, default_time))
                let game_name = client_id ++ "@" ++ color ++ "-" ++ other_id
                let path = log_path ++ "/" ++ show game_id
                game_log <- liftIO $ openFile path WriteMode
                liftIO $ withLogDo game_log $ do
                  time <- liftIO $ getClockTime
                  let date = calendarTimeToString $ toUTCTime time
                  logMsg $ show game_id  ++ " " ++ date
                  logMsg game_name
                  doGame p1 p2
                liftIO $ hClose h
                liftIO $ hClose other_h
                logMsg $ "client " ++ client_id ++ " closes"
            let clean_up e = do
                logMsg $ "game " ++ client_id ++
                         " incurs IO error: " ++ show (e :: IOException)
                liftIO $ do
                  hPutStrLn h "opponent incurred fatal IO error: exiting"
                  hClose h
                  hPutStrLn other_h "opponent incurred fatal IO error: exiting"
                  hClose other_h
            catchLogIO run_game clean_up
          False ->
            liftIO $ hPutStrLn h $ "bad color " ++ color
        where
          valid_color "W" = True
          valid_color "B" = True
          valid_color _ = False
      ["list"] -> liftIO $ do
        ServiceState _ game_list <- readMVar state
        mapM_ output_game game_list
        where
          output_game (GameResv _ game_id other_name color _) =
            hPutStrLn h $ show game_id ++ " " ++ other_name ++ " " ++ [color]
      ["accept", id] -> do
        ServiceState game_id game_list <- liftIO $ takeMVar state
        case parse_int id of
          Nothing -> liftIO $ do
            hPutStrLn h "bad game id"
            putMVar state $ ServiceState game_id game_list
          Just ask_id -> 
            case find_game game_list of
              Nothing -> liftIO $ do
                hPutStrLn h $ "no such game"
                putMVar state $ ServiceState game_id game_list
              Just (other_name, color, wakeup, game_list') ->  do
                logMsg $ "client " ++ client_id ++
                         " accepts " ++ show other_name
                liftIO $ do
                  writeIORef continue False
                  putMVar state $ ServiceState game_id game_list'
                  my_name <- readIORef me
                  writeChan wakeup $ Wakeup my_name client_id h
            where
              find_game game_list = go [] game_list where
                go _ [] = Nothing
                go first this@(GameResv other_name _ _ color wakeup : rest)
                   | game_id == ask_id =
                       Just (other_name, color, wakeup, first ++ rest)
                   | otherwise = go (first ++ this) rest
      _ -> liftIO $ hPutStrLn h $ "unknown command"
