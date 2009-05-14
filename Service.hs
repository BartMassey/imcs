--- Copyright © 2009 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "MIT License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

module Service (
  ServiceState,
  initServiceDir,
  initServiceState,
  doCommands) where

import Prelude hiding (catch)

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception (evaluate)
import Control.Monad
import Data.Char
import Data.IORef
import Data.List
import Data.Ord
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error
import System.Posix.Files
import System.Posix.Directory
import System.Time

import Game
import Log
import Rating
import Version

state_path :: FilePath
state_path = "imcsd"

version_path :: FilePath
version_path = state_path </> "VERSION"

private_path :: FilePath
private_path = state_path </> "private"

game_id_path :: FilePath
game_id_path = private_path </> "GAMEID"

pwf_path :: FilePath
pwf_path = private_path </> "passwd"

pwf_tmp_path :: FilePath
pwf_tmp_path = private_path </> "passwd.tmp"

log_path :: FilePath
log_path = state_path </> "log"

default_time :: Maybe Int
default_time = Just 300000

data Wakeup =
    Wakeup {
      wakeup_name :: String,
      wakeup_client_id :: String,
      wakeup_handle :: Handle }
  | Nevermind

data GamePost = GameResv {
      game_resv_game_id :: Int,
      game_resv_name :: String,
      game_resv_client_id :: String,
      game_resv_side :: Char,
      game_resv_rating :: Rating,
      game_resv_wakeup :: Chan Wakeup }

data PWFEntry = PWFEntry {
      pwf_entry_name :: String,
      pwf_entry_password :: String,
      pwf_entry_rating :: Rating }

data ServiceState = ServiceState {
      service_state_game_id :: Int,
      service_state_game_list :: [GamePost],
      service_state_pwf :: [PWFEntry] }

initServiceDir :: IO ()
initServiceDir = do
  fversion <- read_versionf
  case fversion of
    "" -> do
      game_id <- read_game_id
      case game_id of
        1 -> fresh
        _ -> to_v2_0
    _ -> return ()
  write_versionf
  where
    to_v2_0 = do
      old_pwf <- read_v1_4_pwf
      let new_pwf = map to_pwe old_pwf
      write_pwf new_pwf
      where
        to_pwe (name, password) = PWFEntry name password baseRating
    fresh = do
      createDirectory state_path 0o755
      createDirectory private_path 0o700
      createDirectory log_path 0o755
      h <- openFile pwf_path AppendMode
      hClose h

read_versionf :: IO String
read_versionf = catch read_version_file give_up where
    read_version_file = do
      vh <- openFile version_path ReadMode
      l <- hGetLine vh
      hClose vh
      return $ unwords $ words l
    give_up _ = do
      return ""

write_versionf :: IO ()
write_versionf = do
  vh <- openFile version_path WriteMode
  hPutStrLn vh version
  hClose vh

write_game_id :: Int -> IO ()
write_game_id game_id = do
  idh <- openFile game_id_path WriteMode
  hPrint idh game_id
  hClose idh

write_pwf :: [PWFEntry] -> IO ()
write_pwf pwf = do
  h <- openFile pwf_tmp_path WriteMode
  let write_pwfe (PWFEntry name password rating) =
        hPutStrLn h $ name ++ " " ++ password ++ " " ++ show rating
  mapM_ write_pwfe pwf
  hClose h
  removeLink pwf_path
  rename pwf_tmp_path pwf_path

read_v1_4_pwf :: IO [(String, String)]
read_v1_4_pwf = do
  h <- openFile pwf_path ReadMode
  s <- hGetContents h
  return $ map (make_pwfe . words) $ lines s
  where
    make_pwfe [name, password] =
        (name, password)
    make_pwfe _ = error "bad password file format: exiting"

read_pwf :: IO [PWFEntry]
read_pwf = do
  h <- openFile pwf_path ReadMode
  s <- hGetContents h
  return $ map (make_pwfe . words) $ lines s
  where
    make_pwfe [name, password, rating] =
        PWFEntry name password (read rating)
    make_pwfe _ = error "bad password file format: exiting"

read_game_id :: IO Int
read_game_id = catch read_game_file give_up where
    read_game_file = do
      idh <- openFile game_id_path ReadMode
      l <- hGetLine idh
      hClose idh
      return $ read l
    give_up _ = do
      write_game_id 1
      return 1

initServiceState :: IO ServiceState
initServiceState = do
  fversion <- read_versionf
  unless (fversion == version)
         (evaluate $ error $ "daemon " ++ version ++
                             ", fs " ++ fversion ++
                             ": exiting")
  next_game_id <- read_game_id
  pwf <- read_pwf
  return $ ServiceState {
    service_state_game_id = next_game_id,
    service_state_game_list = [],
    service_state_pwf = pwf }

parse_int :: String -> Maybe Int
parse_int "" = Nothing
parse_int s
    | length s < 9 && all isDigit s = Just (read s)
    | otherwise = Nothing

while :: IORef Bool -> LogIO () -> LogIO ()
while b a = do
  cond <- liftIO $ readIORef b
  case cond of
    False -> return ()
    True -> a >> while b a

pw_lookup :: ServiceState -> String -> Maybe (String, Rating)
pw_lookup ss name = lookup name pwf where
    pwf = map (\(PWFEntry n p r) -> (n, (p, r))) $ service_state_pwf ss

doCommands :: (Handle, String) -> MVar ServiceState -> LogIO ()
doCommands (h, my_client_id) state = do
  liftIO $ do
    ServiceState my_game_id game_list pwf <- takeMVar state
    game_list' <- filterM close_redundant game_list
    let ss' = ServiceState my_game_id game_list' pwf
    putMVar state ss'
  serve h my_client_id state
  where
    close_redundant gr@(GameResv _ _ client_id _ _ wakeup)
        | client_id == my_client_id = do
            writeChan wakeup Nevermind
            return False
        | otherwise = return True

serve :: Handle -> String -> MVar ServiceState -> LogIO ()
serve h client_id state = do
  liftIO $ hPutStrLn h $ "100 imcs " ++ version
  continue <- liftIO $ newIORef True
  me <- liftIO $ newIORef Nothing
  while continue $ do
    line <- liftIO $ hGetLine h
    case words line of
      [] -> do
        return ()
      ["help"] -> do
        liftIO $ hPutStr h $ unlines [
          "210 imcs " ++ version ++ " help",
          " help: this help",
          " quit: quit imcs",
          " me <name> <password>: log in",
          " register <name> <password>: register a new name and log in",
          " password <password>: change password",
          " list: list available games in format",
          "       <id> <opponent-name> <opponent-color> <opponent-rating>",
          " ratings: list player ratings (top 10 plus own)",
          " offer <color>: offer a game as W or B",
          " accept <id>: accept a game with an opponent",
          "." ]
      ["quit"] -> do
        logMsg $ "client " ++ client_id ++ " quits"
        liftIO $ do
          hPutStrLn h "200 Goodbye"
          writeIORef continue False
          hClose h
      ["me", name, password] -> do
        ss <- liftIO $ readMVar state
        case pw_lookup ss name of
          Nothing ->
              liftIO $ hPutStrLn h $ "400 no such username: " ++
                                     "please use register command"
          Just (password', rating') ->
              if password /= password'
              then
                liftIO $ hPutStrLn h "401 wrong password"
              else do
                logMsg $ "client " ++ client_id ++ " aka " ++ name
                liftIO $ writeIORef me $ Just name
                liftIO $ hPutStrLn h $ "201 hello " ++ name
      ["register", name, password] -> do
         ss <- liftIO $ takeMVar state
         case pw_lookup ss name of
           Just _ -> liftIO $ do
             hPutStrLn h $ "402 username already exists: " ++
                           "please use password command to change"
             putMVar state ss
           Nothing -> do
             logMsg $ "registering client " ++ client_id ++
                      " as user " ++ name
             let ServiceState game_id game_list pwf = ss
             let pwfe = PWFEntry name password baseRating
             let pwf' = pwf ++ [pwfe]
             let ss' = ServiceState game_id game_list pwf'
             liftIO $ do
               write_pwf pwf'
               putMVar state ss'
               writeIORef me $ Just name
               hPutStrLn h $ "202 hello new user " ++ name
      ["password", password] -> do
        maybe_my_name <- liftIO $ readIORef me
        case maybe_my_name of
          Nothing ->
            liftIO $ hPutStrLn h "403 please use the me command first"
          Just my_name -> do
            ss <- liftIO $ takeMVar state
            case pw_lookup ss my_name of
              Nothing -> do
                logMsg $ "client " ++ client_id ++ " name " ++
                         my_name ++ "not in password file"
                liftIO $ putMVar state ss
                liftIO $ hPutStrLn h "500 you do not exist: go away"
              Just _ -> do
                logMsg $ "changing password for client " ++ client_id ++
                         " user " ++ my_name
                let ServiceState game_id game_list pwf = ss
                let newpass e@(PWFEntry n _ rating)
                     | n == my_name = PWFEntry n password rating
                     | otherwise = e
                let pwf' = map newpass pwf
                let ss' = ServiceState game_id game_list pwf'
                liftIO $ do
                  write_pwf pwf'
                  putMVar state ss'
                  hPutStrLn h $ "203 password change for user " ++ my_name
      ["list"] -> liftIO $ do
        ServiceState _ game_list _ <- readMVar state
        hPutStrLn h "211 available games"
        mapM_ output_game game_list
        hPutStrLn h "."
        where
          output_game (GameResv game_id other_name _ color rating _) =
            hPutStrLn h $ " " ++ show game_id ++
                          " " ++ other_name ++
                          " " ++ [color] ++
                          " " ++ show rating
      ["ratings"] -> liftIO $ do
        ServiceState _ _ pwf <- readMVar state
        maybe_my_name <- readIORef me
        let ratings = map rating_info pwf
        let top10 = take 10 $ sortBy (comparing (negate . snd)) ratings
        let rating_list =
                case maybe_my_name of
                  Nothing -> top10
                  Just my_name ->
                      case lookup my_name top10 of
                        Just _ -> top10
                        Nothing ->
                            case lookup my_name ratings of
                              Nothing -> top10
                              Just my_rating -> top10 ++ [(my_name, my_rating)]
        hPutStrLn h "212 ratings"
        mapM_ output_rating rating_list
        hPutStrLn h "."
        where
          rating_info (PWFEntry pname _ prating) = (pname, prating)
          output_rating (pname, prating) =
            hPutStrLn h $ " " ++ pname ++
                          " " ++ show prating
      ["offer", color] -> do
        maybe_my_name <- liftIO $ readIORef me
        case (maybe_my_name, valid_color color) of
          (Nothing, _) ->
            liftIO $ hPutStrLn h "404 must set name first using me command"
          (_, False) ->
            liftIO $ hPutStrLn h $ "405 bad color " ++ color
          (Just my_name, True) -> do
            logMsg $ "client " ++ client_id ++ " offers game as " ++ color
            wakeup <- liftIO $ newChan
            ss <- liftIO $ takeMVar state
            let ServiceState game_id game_list pwf = ss
            let my_rating =
                  case pw_lookup ss my_name of
                    Just (_, r) -> r
                    --- XXX shouldn't normally happen
                    Nothing -> baseRating
            let new_game =
                  GameResv game_id my_name client_id (head color)
                           my_rating wakeup
            let service_state' =
                  ServiceState (game_id + 1) (game_list ++ [new_game]) pwf
            liftIO $ do
              write_game_id (game_id + 1)
              putMVar state service_state'
              hPutStrLn h $ "101 game " ++ show game_id ++
                            " waiting for offer acceptance"
            w <- liftIO $ readChan wakeup
            case w of
              Wakeup other_name other_id other_h -> do
                liftIO $ hPutStrLn h "102 received acceptance"
                liftIO $ writeIORef continue False
                let run_game = do
                    let game_desc = show game_id ++ ": " ++
                                    my_name ++ "(" ++ client_id ++
                                    ", " ++ color ++ ") vs " ++
                                    other_name ++ "(" ++ other_id ++ ")"
                    logMsg $ "game " ++ game_desc ++ " begins"
                    let ((p1, p1_name), (p2, p2_name)) = 
                          case head color of
                            'W' -> (((h, default_time), my_name),
                                    ((other_h, default_time), other_name))
                            'B' -> (((other_h, default_time), other_name),
                                    ((h, default_time), my_name))
                            _   -> error "internal error: bad color"
                    let path = log_path </> show game_id
                    game_log <- liftIO $ openFile path WriteMode
                    liftIO $ withLogDo game_log $ do
                      time <- liftIO $ getClockTime
                      let date = calendarTimeToString $ toUTCTime time
                      logMsg game_desc
                      logMsg date
                      score <- doGame p1 p2
                      liftIO $ do
                        p1_rating <- lookup_rating p1_name
                        p2_rating <- lookup_rating p2_name
                        update_rating p1_name p1_rating p2_rating score
                        update_rating p2_name p2_rating p1_rating (-score)
                        hClose h
                        hClose other_h
                      logMsg $ "client " ++ client_id ++ " closes"
                      where
                        lookup_rating name = do
                          ss <- readMVar state
                          case pw_lookup ss name of
                            Just (_, rating) -> return rating
                            --- XXX should never happen
                            Nothing -> return baseRating
                        update_rating name ra rb s = do
                          let ra' = updateRating ra rb s
                          ss <- takeMVar state
                          let ServiceState game_id game_list pwf = ss
                          let newpass e@(PWFEntry n password _)
                                | n == name = PWFEntry n password ra'
                                | otherwise = e
                          let pwf' = map newpass pwf
                          write_pwf pwf'
                          let ss' = ServiceState game_id game_list pwf'
                          putMVar state ss'
                let clean_up e = do
                    logMsg $ "game " ++ client_id ++
                             " incurs IO error: " ++ show e
                    liftIO $ do
                      close_it h
                      close_it other_h
                    where
                      close_it h = 
                        catch (do
                                  hPutStrLn h "420 fatal IO error: exiting"
                                  hClose h)
                              (\_ -> return ())
                catchLogIO run_game clean_up
              Nevermind ->
                alsoLogMsg h "421 offer countermanded"
        where
          valid_color "W" = True
          valid_color "B" = True
          valid_color _ = False
      ["accept", accept_game_id] -> do
        maybe_my_name <- liftIO $ readIORef me
        case (parse_int accept_game_id, maybe_my_name) of
          (_, Nothing) ->
            liftIO $ hPutStrLn h "406 must set name first using me command"
          (Nothing, _) ->
            liftIO $ hPutStrLn h "407 bad game id"
          (Just ask_id, Just my_name) -> do
            ServiceState game_id game_list pwf <- liftIO $ takeMVar state
            case find_game game_list of
              Nothing -> liftIO $ do
                putMVar state $ ServiceState game_id game_list pwf
                hPutStrLn h $ "408 no such game"
              Just (other_name, wakeup, game_list') ->  do
                logMsg $ "client " ++ client_id ++
                         " accepts " ++ show other_name
                liftIO $ do
                  writeIORef continue False
                  putMVar state $ ServiceState game_id game_list' pwf
                  hPutStrLn h "103 accepting offer"
                  writeChan wakeup $ Wakeup my_name client_id h
            where
              find_game game_list =
                  case partition ((== ask_id) . game_resv_game_id) game_list of
                    ([GameResv game_id' other_name _ _ _ wakeup], rest) ->
                        Just (other_name, wakeup, rest)
                    ([], _) -> Nothing
                    _ -> error $ "internal error: multiple games " ++
                                 "with same id in list"
      _ -> liftIO $ hPutStrLn h $ "501 unknown command"
