--- Copyright © 2009 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "MIT License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

module Service (
  ServiceState,
  initService,
  upgradeService,
  initServiceState,
  doCommands) where

import Prelude hiding (catch)

import Control.Concurrent
import Control.Exception (evaluate, throw)
import Control.Monad
import Control.Monad.Error
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import Data.Ord
import Network
import Random
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error
import System.Posix.Files
import System.Posix.Directory
import System.Time
import Text.Printf
import Text.Regex.Posix

import Game
import Log
import Rating
import SNewLine
import Version

debugExpectSend :: Bool
debugExpectSend = False

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

default_time :: Int
default_time = 300000

data Wakeup =
    Wakeup {
      _wakeup_name :: String,
      _wakeup_client_id :: String,
      _wakeup_handle :: Handle,
      _wakeup_color :: String }
  | Nevermind

data GamePost =
    GameResv {
      game_resv_game_id :: Int,
      game_resv_name :: String,
      _game_resv_client_id :: String,
      _game_resv_side :: Char,
      _game_resv_time_controls :: (Int, Int),
      game_resv_wakeup :: Chan Wakeup }
  | InProgress {
      _in_progress_game_id :: Int,
      _in_progress_name_white :: String,
      _in_progress_name_black :: String,
      in_progress_wakeup :: MVar () }

data PWFEntry = PWFEntry {
      _pwf_entry_name :: String,
      _pwf_entry_password :: String,
      _pwf_entry_rating :: Rating }

data ServiceState = ServiceState {
      service_state_game_id :: Int,
      service_state_game_list :: [GamePost],
      service_state_pwf :: [PWFEntry] }

createVersion :: IO ()
createVersion = do
  createDirectory state_path 0o755
  createDirectory private_path 0o700
  createDirectory log_path 0o755
  h <- openFile pwf_path AppendMode
  hClose h
  write_versionf

initService :: Int -> IO ()
initService port = do
  catch try_to_connect (\_ -> return ())
  createVersion
  where
    try_to_connect = do
      h <- connectTo "127.0.0.1" $ PortNumber $ fromIntegral port
      hClose h
      error "cannot init: server is running"

upgradeService :: Int -> Maybe String -> IO ()
upgradeService = touchService False

touchService :: Bool -> Int -> Maybe String -> IO ()
touchService new_ok port opt_admin_pw = do
  fversion <- read_versionf
  case fversion of
    "2.5" -> do
      putStrLn $ "using existing version " ++ fversion
      terminate_existing_server
    "2.4" -> do
      terminate_existing_server
      write_versionf
    "2.3" -> do
      terminate_existing_server
      write_versionf
    "2.2" -> do
      terminate_existing_server
      write_versionf
    "2.1" -> do
      terminate_existing_server
      write_versionf
    "2.0" -> do
      write_versionf
    "" -> do
      check_new_ok
      to_v2_0
      write_versionf
    v -> error $ "unknown version " ++ v
  where
    check_new_ok
      | new_ok = return ()
      | otherwise = error "refusing to upgrade from old version"
    to_v2_0 = do
       game_id <- read_game_id
       case game_id of
         1 -> to_v2_0_from_v1_0
         _ -> to_v2_0_from_v1_4
       write_versionf
    to_v2_0_from_v1_0 =
      createVersion
    to_v2_0_from_v1_4 = do
       old_pwf <- read_v1_4_pwf
       let new_pwf = map to_pwe old_pwf
       write_pwf new_pwf
       where
         to_pwe (name, password) = PWFEntry name password baseRating
    terminate_existing_server =
      case opt_admin_pw of
        Nothing -> return ()
        Just admin_pw -> do
          let try_to_connect = do
                h <- connectTo "127.0.0.1" $ PortNumber $ fromIntegral port
                let expect code failure = do
                      when debugExpectSend $ putStrLn $ "expecting " ++ code
                      let process_line = do
                            line <- hGetLine h
                            when debugExpectSend $ putStrLn $ "got " ++ line
                            case words line of
                              (code' : _) | code == code' -> return ()
                              _ ->  do 
                                let msg = failure ++ ": " ++ line
                                let e = mkIOError userErrorType
                                                  msg (Just h) Nothing
                                throw e
                      let io_fail e = do
                            putStrLn $ "IO Error: exiting"
                            throw e
                      catch process_line io_fail
                let send s = do
                      when debugExpectSend $ putStrLn $ "sending " ++ s
                      hPutStrLn h s
                expect "100" "unexpected server hello"
                send $ "me admin " ++ admin_pw
                expect "201" "could not become admin"
                send "stop"
                expect "104" "server did not seem to be stopping"
                expect "205" "server cannot or would not stop"
                hClose h
                return ()
          let fail_to_connect e = do
                putStrLn $ "could not find server, continuing upgrade:" ++
                           show port ++ " : " ++ show e
          catch try_to_connect fail_to_connect

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

pw_lookup :: ServiceState -> String -> Maybe (String, Rating)
pw_lookup ss name = lookup name pwf where
    pwf = map (\(PWFEntry n p r) -> (n, (p, r))) $ service_state_pwf ss

type ELIO = ErrorT () LogIO

instance Error ()

finish :: ELIO ()
finish = throwError ()

game_lookup :: [GamePost] -> Int
            -> ELIO (Either String 
                     (GamePost, [GamePost]))
game_lookup game_list game_id = do
  case partition waiting_game game_list of
    ([g], rest) ->
        return $ Right (g, rest)
    ([], _) -> 
        return $ Left "408 no such game"
    _ ->
        return $ Left "499 internal error: multiple games with same id in list"
  where
    waiting_game (GameResv {game_resv_game_id = game_id'}) 
      | game_id' == game_id = True
    waiting_game _ = False

data CommandState = CS {
      cs_main_thread :: ThreadId,
      cs_reaccept :: MVar Bool,
      cs_h :: Handle,
      cs_client_id :: String,
      cs_state :: MVar ServiceState,
      cs_me :: IORef (Maybe String) }

type Command = CommandState -> ELIO ()
    
helpCommand :: Command
helpCommand (CS {cs_h = h, cs_me = me}) = do
  maybe_my_name <- liftIO $ readIORef me
  liftIO $ hPutStr h $ sUnlines $ [
    "210 imcs " ++ version ++ " help",
    " help: this help",
    " quit: quit imcs",
    " me <name> <password>: log in",
    " register <name> <password>: register a new name and log in",
    " password <password>: change password",
    " list: list available games in format",
    "        <id> <opponent-name> <opponent-color> <opponent-rating>",
    " ratings: list player ratings (top 10 plus own)",
    " offer [<color>] [<time> [<time>]]: offer a game with given constraints",
    " accept <id> [<color>]: accept a game with an opponent",
    " clean: cancel all outstanding offers of current player" ] ++
    (case maybe_my_name of
       Just "admin" -> [" stop: stop the server"]
       _ -> []) ++ ["."]

quitCommand :: Command
quitCommand (CS {cs_h = h, cs_client_id = client_id}) = do
  logMsg $ "client " ++ client_id ++ " quits"
  sPutStrLn h "200 Goodbye"
  liftIO $ hClose h
  finish

meCommand :: String -> String -> Command
meCommand name password (CS {cs_h = h,  cs_client_id = client_id,
                             cs_state = state, cs_me = me}) = do
  ss <- liftIO $ readMVar state
  case pw_lookup ss name of
    Nothing ->
        sPutStrLn h $ "400 no such username: " ++
                               "please use register command"
    Just (password', _) ->
        if password /= password'
        then
          sPutStrLn h "401 wrong password"
        else do
          logMsg $ "client " ++ client_id ++ " aka " ++ name
          liftIO $ writeIORef me $ Just name
          sPutStrLn h $ "201 hello " ++ name

registerCommand :: String -> String -> Command
registerCommand name password (CS {cs_h = h, cs_client_id = client_id,
                                   cs_state = state, cs_me = me}) = do
  ss <- liftIO $ takeMVar state
  case pw_lookup ss name of
    Just _ -> do
      liftIO $ putMVar state ss
      sPutStrLn h $ "402 username already exists: " ++
                    "please use password command to change"
    Nothing -> do
      let ServiceState game_id game_list pwf = ss
      let pwfe = PWFEntry name password baseRating
      let pwf' = pwf ++ [pwfe]
      let ss' = ServiceState game_id game_list pwf'
      liftIO $ do
        write_pwf pwf' --- XXX failure will hang server
        putMVar state ss'
        writeIORef me $ Just name
      sPutStrLn h $ "202 hello new user " ++ name
      logMsg $ "registered client " ++ client_id ++
               " as user " ++ name

passwordCommand :: String -> Command
passwordCommand password (CS {cs_h = h, cs_client_id = client_id,
                              cs_state = state, cs_me = me}) = do
  maybe_my_name <- liftIO $ readIORef me
  case maybe_my_name of
    Nothing ->
      sPutStrLn h "403 please use the me command first"
    Just my_name -> do
      ss <- liftIO $ takeMVar state
      case pw_lookup ss my_name of
        Nothing -> do
          liftIO $ putMVar state ss
          logMsg $ "client " ++ client_id ++ " name " ++
                   my_name ++ "not in password file"
          sPutStrLn h "500 you do not exist: go away"
        Just _ -> do
          let ServiceState game_id game_list pwf = ss
          let newpass e@(PWFEntry n _ rating)
               | n == my_name = PWFEntry n password rating
               | otherwise = e
          let pwf' = map newpass pwf
          let ss' = ServiceState game_id game_list pwf'
          liftIO $ do
            write_pwf pwf'  --- XXX failure will hang server
            putMVar state ss'
          sPutStrLn h $ "203 password change for user " ++ my_name
          logMsg $ "changed password for client " ++ client_id ++
                   " user " ++ my_name

show_time :: Int -> String
show_time t = 
  printf "%d:%02d" (t `div` 60000) ((t `div` 1000) `mod` 60)

listCommand :: Command
listCommand (CS {cs_h = h, cs_state = state}) = do
  ss@(ServiceState _ game_list _) <- liftIO $ readMVar state
  sPutStrLn h $ printf "211 %d available games" (length game_list)
  let lookup_rating other_name =
          case pw_lookup ss other_name of
            Nothing -> "?"
            Just (_, r) -> show r
  let output_game (GameResv game_id other_name _ color (t1, t2) _) =
          sPutStrLn h $ " " ++ show game_id ++
                        " " ++ other_name ++
                        " " ++ [color] ++
                        " " ++ show_time t1 ++
                        " " ++ show_time t2 ++
                        " " ++ lookup_rating other_name ++
                        " [offer]"
      output_game (InProgress game_id name_white name_black _) =
          sPutStrLn h $ " " ++ show game_id ++
                        " " ++ name_white ++
                        " " ++ name_black ++
                        " (" ++ lookup_rating name_white ++ "/" ++
                             lookup_rating name_black ++ ") " ++
                        " [in-progress]"
  mapM_ output_game game_list
  sPutStrLn h "."

ratingsCommand :: Command
ratingsCommand (CS {cs_h = h, cs_state = state, cs_me = me}) = do
  ServiceState _ _ pwf <- liftIO $ readMVar state
  maybe_my_name <- liftIO $ readIORef me
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
  sPutStrLn h "212 ratings"
  mapM_ output_rating rating_list
  sPutStrLn h "."
  where
    rating_info (PWFEntry pname _ prating) = (pname, prating)
    output_rating (pname, prating) =
      sPutStrLn h $ " " ++ pname ++
                    " " ++ show prating

check_color :: Maybe String -> Maybe String
check_color Nothing = Just "?"
check_color (Just "W") = Just "W"
check_color (Just "B") = Just "B"
check_color (Just "w") = Just "W"
check_color (Just "b") = Just "B"
check_color (Just "?") = Just "?"
check_color (Just _) = Nothing

offerCommand' :: Maybe String -> Maybe (Int, Int) -> Command
offerCommand' opt_color opt_times 
  (CS {cs_h = h, cs_client_id = client_id,
       cs_state = state, cs_me = me}) = do
  case check_color opt_color of
    Nothing ->
      sPutStrLn h $ "405 bad color " ++ fromJust opt_color
    Just my_color -> do
      maybe_my_name <- liftIO $ readIORef me
      case maybe_my_name of
        Nothing ->
          sPutStrLn h "404 must set name first using me command"
        Just my_name -> do
          logMsg $ "client " ++ client_id ++
                   " offers game as " ++ my_color
          wakeup <- liftIO $ newChan
          ss <- liftIO $ takeMVar state
          let ServiceState game_id game_list pwf = ss
          let new_game =
                GameResv game_id my_name client_id (head my_color) tc wakeup
          let service_state' =
                ServiceState (game_id + 1) (game_list ++ [new_game]) pwf
          liftIO $ do
            write_game_id (game_id + 1)   --- XXX failure hangs server

            putMVar state service_state'
            let result_code = case my_color of
                                "?" -> "103 "
                                "W" -> "107 "
                                "B" -> "108 "
                                _ -> error "internal error: unexpected color"
            hPutStrLn h $ result_code ++ my_color ++ " game " ++
                          show game_id ++
                          " waiting for offer acceptance"
          w <- liftIO $ readChan wakeup
          case w of
            Wakeup other_name other_id other_h other_color -> do
              sPutStrLn h "102 received acceptance"
              let my_info = ((h, Just my_time),
                             my_name, my_color)
              let other_info = ((other_h, Just other_time),
                                other_name, other_color)
              let ((p1, p1_name), (p2, p2_name)) =
                    sort_colors my_info other_info
              wchan <- liftIO $ newEmptyMVar
              let ip = InProgress game_id p1_name p2_name wchan
              (ServiceState game_id'' game_list'' pwf'') <-
                liftIO $ takeMVar state
              let gl' = game_list'' ++ [ip]
              liftIO $ putMVar state (ServiceState game_id'' gl' pwf'')
              let run_game = do
                    let game_desc = show game_id ++ ": " ++
                                      my_name ++ "(" ++ client_id ++
                                      ", " ++ my_color ++ ") vs " ++
                                      other_name ++
                                      "(" ++ other_id ++ ")"
                    logMsg $ "game " ++ game_desc ++ " begins"
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
                      update_rating p1_name p1_rating
                        p2_rating score
                      update_rating p2_name p2_rating
                        p1_rating (-score)
                      hClose h
                      hClose other_h
                    logMsg $ "client " ++ client_id ++ " closes"
                    where
                      lookup_rating name = do
                        ss' <- readMVar state
                        case pw_lookup ss' name of
                          Just (_, rating) -> return rating
                          --- XXX should never happen
                          Nothing -> return baseRating
                      update_rating name ra rb s = do
                        let ra' = updateRating ra rb s
                        ss' <- takeMVar state
                        let ServiceState game_id''' game_list''' pwf''' = ss'
                        let newpass e@(PWFEntry n password _)
                              | n == name = PWFEntry n password ra'
                              | otherwise = e
                        let pwf' = map newpass pwf'''
                        let ss'' = ServiceState game_id''' game_list''' pwf'
                        write_pwf pwf'   --- XXX failure hangs server
                        putMVar state ss''
              let clean_up e = do
                    logMsg $ "game " ++ client_id ++
                      " incurs IO error: " ++ show e
                    liftIO $ close_it h
                    liftIO $ close_it other_h
                    where
                      close_it :: Handle -> IO ()
                      close_it h' = 
                        catch (do
                                  hPutStrLn h'
                                    "420 fatal IO error: exiting\r\n"
                                  hClose h')
                              (\_ -> return ())
              lift $ catchLogIO run_game clean_up
              (ServiceState game_id''' game_list''' pwf''') <-
                liftIO $ takeMVar state
              let exclude_me (InProgress game_id' _ _ _)
                    | game_id == game_id' = False
                  exclude_me _ = True
              let gl''' = filter exclude_me game_list'''
              liftIO $ putMVar state
                (ServiceState game_id''' gl''' pwf''')
              liftIO $ putMVar wchan ()
              finish
            Nevermind ->
              alsoLogMsg h "421 offer countermanded"
      where
        sort_colors (player_l, name_l, color_l)
                    (player_r, name_r, color_r) =
            case (color_l, color_r) of
              ("W", "B") -> left
              ("B", "W") -> right
              ("?", "B") -> left
              ("?", "W") -> right
              ("W", "?") -> left
              ("B", "?") -> right
              colors -> error $ "internal error: bad offer colors " ++ 
                                show colors
            where
              left = ((player_l, name_l), (player_r, name_r))
              right = ((player_r, name_r), (player_l, name_l))
  where
    tc@(my_time, other_time) =
      case opt_times of
        Just ts -> ts
        Nothing -> (default_time, default_time)


offerCommand :: [String] -> Command
offerCommand args cs = do
  let (opt_color, rest) = parse_color args
  let maybe_times = rest >>= parse_times
  case maybe_times of
    Just opt_times -> offerCommand' opt_color opt_times cs
    _ -> liftIO $ hPutStrLn (cs_h cs) "409 bad argument(s)"
  where
    parse_color [] = (Nothing, Just [])
    parse_color args'@(arg : rest) =
      case check_color (Just arg) of
        Just c -> (Just c, Just rest)
        Nothing -> (Nothing, Just args')
    parse_times [] = 
      return Nothing
    parse_times [a] = do
      t <- parse_time a
      return $ Just (t, t)
    parse_times [a1, a2] = do
      t1 <- parse_time a1
      t2 <- parse_time a2
      return $ Just (t1, t2)
    parse_times _ = Nothing
    parse_time a =
      case getAllTextSubmatches (a =~ timeRE) of
        [] -> Nothing
        [_, f1, _, f2] ->
          case f2 of
            "" -> Just (1000 * read f1)
            _ -> Just (60000 * read f1 + 1000 * read f2)
        _ -> error "internal error: parse_time weirdness"
      where
        timeRE = "^([0-9]+)(:([0-9][0-9]))?$"

acceptCommand :: String -> Maybe String -> Command
acceptCommand accept_game_id opt_color
              (CS {cs_h = h, cs_client_id = client_id,
                   cs_state = state, cs_me = me}) = do
  case check_color opt_color of
    Nothing ->
      sPutStrLn h $ "405 bad color " ++ fromJust opt_color
    Just my_color -> do
      maybe_my_name <- liftIO $ readIORef me
      case (parse_int accept_game_id, maybe_my_name) of
        (_, Nothing) ->
          sPutStrLn h "406 must set name first using me command"
        (Nothing, _) ->
          sPutStrLn h "407 bad game id"
        (Just ask_id, Just my_name) -> do
          ss@(ServiceState game_id game_list pwf) <-
              liftIO $ takeMVar state
          game <- game_lookup game_list ask_id
          case game of
            Left err -> do
              liftIO $ putMVar state ss
              alsoLogMsg h err
            Right (GameResv _ other_name _ other_color 
                            (other_time, my_time) wakeup, 
                   game_list') ->  do
              side <-
                case (my_color, other_color) of
                  ("W", 'B') -> return $ Just ("105", "W")
                  ("B", 'W') -> return $ Just ("106", "B")
                  ("?", 'B') -> return $ Just ("105", "W")
                  ("?", 'W') -> return $ Just ("106", "B")
                  ("W", '?') -> return $ Just ("105", "W")
                  ("B", '?') -> return $ Just ("106", "B")
                  ("W", 'W') -> return $ Nothing
                  ("B", 'B') -> return $ Nothing
                  ("?", '?') -> do
                    dirn <- liftIO $ randomRIO (0, 1::Int)
                    case dirn of
                      0 -> return $ Just ("105", "W")
                      1 -> return $ Just ("106", "B")
                      _ -> error "internal error: couldn't choose side"
                  colors -> error $ 
                              "internal error: bad accept colors " ++ 
                              show colors
              case side of
                Just (code, my_color') -> do
                  liftIO $ putMVar state $
                    ServiceState game_id game_list' pwf
                  logMsg $ "client " ++ client_id ++
                    " accepts " ++ show other_name
                  liftIO $ do
                    hPutStrLn h $ 
                      code ++ " " ++ my_color' ++ 
                      " " ++ show_time my_time ++ " " ++
                      show_time other_time ++ " accepting offer"
                    writeChan wakeup $ Wakeup my_name client_id h my_color'
                  finish
                Nothing -> do
                  liftIO $ putMVar state ss
                  alsoLogMsg h $ "405 bad color " ++ my_color
            _ -> error "internal error: bad game lookup"

cleanCommand :: Command
cleanCommand (CS {cs_h = h, cs_state = state, cs_me = me}) = do
  maybe_my_name <- liftIO $ readIORef me
  case maybe_my_name of
    Nothing -> sPutStrLn h "406 must set name first using me command"
    Just my_name -> do
      ServiceState game_id game_list pwf <- liftIO $ takeMVar state
      let (my_list, other_list) = partition my_game game_list
      let ss' = ServiceState game_id other_list pwf
      liftIO $ putMVar state ss'
      liftIO $ mapM_ close_game my_list
      sPutStrLn h $ "204 " ++ show (length my_list) ++
                             " games cleaned"
      where
        my_game (GameResv { game_resv_name = name })
            | name == my_name = True
        my_game _ = False
        close_game gr = writeChan (game_resv_wakeup gr) Nevermind

stopCommand :: Command
stopCommand (CS {cs_h = h, cs_state = state, cs_me = me,
                 cs_client_id = client_id,
                 cs_main_thread = main_thread,
                 cs_reaccept = reaccept}) = do
  maybe_my_name <- liftIO $ readIORef me
  case maybe_my_name of
    Nothing -> sPutStrLn h "406 must set name first using me command"
    Just "admin" -> do
      logMsg $ "stopping server for " ++ client_id
      sPutStrLn h "104 stopping server"
      liftIO $ do
        ServiceState game_id game_list pwf <- takeMVar state
        _ <- takeMVar reaccept
        putMVar reaccept False
        let ss' = ServiceState game_id [] pwf
        putMVar state ss'
        mapM_ close_game game_list
      sPutStrLn h "205 server stopped"
      liftIO $ hClose h
      liftIO $ throwTo main_thread ExitSuccess
      finish                                                       
      where
        close_game (GameResv { game_resv_wakeup = w }) =
            writeChan w Nevermind
        close_game (InProgress { in_progress_wakeup = w }) = do
            readMVar w
    Just _ -> sPutStrLn h "502 admin only"

doCommands :: (ThreadId, MVar Bool) -> (Handle, String)
           -> MVar ServiceState -> LogIO ()
doCommands (main_thread, reaccept) (h, client_id) state = do
  sPutStrLn h $ "100 imcs " ++ version
  me <- liftIO $ newIORef Nothing
  let params = CS main_thread reaccept h client_id state me
  _ <- runErrorT $ forever $ do
    line <- liftIO $ hGetLine h
    case words line of
      [] -> return ()
      ["help"] -> helpCommand params
      ["quit"] -> quitCommand params
      ["me", name, password] -> meCommand name password params
      ["register", name, password] -> registerCommand name password params
      ["password", password] -> passwordCommand password params
      ["list"] -> listCommand params
      ["ratings"] -> ratingsCommand params
      ("offer" : args) -> offerCommand args params
      ("accept" : accept_game_id : opt_color) | length opt_color <= 1 ->
        acceptCommand accept_game_id (listToMaybe opt_color) params
      ["clean"] -> cleanCommand params
      ["stop"] -> stopCommand params
      _ -> sPutStrLn h $ "501 unknown command"
  return ()
