--- Copyright © 2009 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "MIT License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

module Service(GameState, doCommands) where

import Control.Monad
import Data.IORef
import System.IO
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan

import Version
import Log
import Game

data GameState = GameResv String Char (Chan Handle)

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
              game_list <- takeMVar state
              my_name <- readIORef me
              let new_game = GameResv my_name (head color) wakeup
              let game_list' = new_game : game_list
              putMVar state game_list'
              writeIORef continue False
            other_h <- liftIO $ readChan wakeup
            case head color of
              'W' -> forkLogIO $ do
                doGame (h, Nothing) (other_h, Nothing)
                liftIO $ hClose h
                liftIO $ hClose other_h
              'B' -> forkLogIO $ do
                doGame (other_h, Nothing) (h, Nothing)
                liftIO $ hClose h
                liftIO $ hClose other_h
            logMsg $ "client " ++ client_id ++ "closes"
          False ->
            liftIO $ hPutStrLn h $ "bad color " ++ color
        where
          valid_color "W" = True
          valid_color "B" = True
          valid_color _ = False
      ["list"] -> liftIO $ do
        game_list <- takeMVar state
        mapM_ output_game game_list
        putMVar state game_list
        where
          output_game (GameResv other_name color _) =
            hPutStrLn h $ " " ++ other_name ++ " " ++ [color]
      ["accept", name] -> do
        game_list <- liftIO $ takeMVar state
        case find_game game_list of
          Nothing -> liftIO $ do
            hPutStrLn h $ "no such game"
            putMVar state game_list
          Just (color, wakeup, game_list') ->  do
            logMsg $ "client " ++ client_id ++ "accepts" ++ name
            liftIO $ do
              writeIORef continue False
              writeChan wakeup h
              putMVar state game_list'
        where
          find_game game_list = go [] game_list where
            go _ [] = Nothing
            go first this@(GameResv other_name color wakeup : rest)
               | other_name == name = Just (color, wakeup, first ++ rest)
               | otherwise = go (first ++ this) rest
      _ -> liftIO $ hPutStrLn h $ "unknown command"
