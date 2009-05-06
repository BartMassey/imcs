--- Copyright © 2009 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "MIT License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

module Service(GameState, doCommands) where

import Control.Exception
import Control.Monad
import Data.IORef
import System.IO
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan

import Version
import Log
import Game

default_time :: Maybe Int
default_time = Just 300000

data GameState = GameResv String String Char (Chan (String, Handle))

while :: IORef Bool -> LogIO () -> LogIO ()
while b a = do
  cond <- liftIO $ readIORef b
  case cond of
    False -> return ()
    True -> a >> while b a

doCommands :: (Handle, String) -> MVar [GameState] -> LogIO ()
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
          " list: list available games",
          " offer <color>: offer a game as W or B",
          " accept <alias>: accept a game with an opponent" ]
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
            liftIO $ do
              writeIORef continue False
              game_list <- takeMVar state
              my_name <- readIORef me
              let new_game = GameResv client_id my_name (head color) wakeup
              let game_list' = new_game : game_list
              putMVar state game_list'
            (other_id, other_h) <- liftIO $ readChan wakeup
            let run_game = do
                logMsg $ "game " ++ client_id ++
                         " (" ++ color ++ ") vs " ++
                         other_id ++ " begins"
                let (p1, p2) = case head color of
                                 'W' -> ((h, default_time),
                                         (other_h, default_time))
                                 'B' -> ((other_h, default_time),
                                         (h, default_time))
                let game_id = client_id ++ "@" ++ color ++ "-" ++ other_id
                let path = "log/" ++ game_id
                game_log <- liftIO $ openFile path WriteMode
                liftIO $ withLogDo game_log $ do
                  logMsg game_id
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
        game_list <- readMVar state
        mapM_ output_game game_list
        where
          output_game (GameResv _ other_name color _) =
            hPutStrLn h $ " " ++ other_name ++ " " ++ [color]
      ["accept", name] -> do
        game_list <- liftIO $ takeMVar state
        case find_game game_list of
          Nothing -> liftIO $ do
            hPutStrLn h $ "no such game"
            putMVar state game_list
          Just (other_id, color, wakeup, game_list') ->  do
            logMsg $ "client " ++ client_id ++ " accepts " ++ other_id
            liftIO $ do
              writeIORef continue False
              putMVar state game_list'
              writeChan wakeup (client_id, h)
        where
          find_game game_list = go [] game_list where
            go _ [] = Nothing
            go first this@(GameResv other_id other_name color wakeup : rest)
               | other_name == name =
                   Just (other_id, color, wakeup, first ++ rest)
               | otherwise = go (first ++ this) rest
      _ -> liftIO $ hPutStrLn h $ "unknown command"
