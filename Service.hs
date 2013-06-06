{-# LANGUAGE CPP #-}
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

#if __GLASGOW_HASKELL__ < 706
import Prelude hiding (catch)
#endif
import Control.Concurrent
import Control.Exception (evaluate, throw)
import Control.Exception.Base (catch, IOException)
import Control.Monad
import Control.Monad.Error
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import Data.Ord
import Network
import System.Random
import System.Exit
import System.FilePath
import System.IO
#if __GLASGOW_HASKELL__ < 706
import System.IO.Error hiding (catch)
#else
import System.IO.Error
#endif
import System.Posix.Files
import System.Posix.Directory
import System.Time
import Text.Printf
import Text.Regex.TDFA

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
      pwf_entry_name :: String,
      _pwf_entry_password :: String,
      pwf_entry_rating :: Rating }

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
  catch try_to_connect ((\_ -> return ()) :: IOException -> IO ())
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
                            line <- sGetLine h
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
                            throw (e :: IOException)
                      catch process_line io_fail
                let send s = do
                      when debugExpectSend $ putStrLn $ "sending " ++ s
                      sPutStrNL h s
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
                           show port ++ " : " ++ show (e :: IOException)
          catch try_to_connect fail_to_connect

read_versionf :: IO String
read_versionf = catch read_version_file give_up where
    read_version_file = do
      vh <- openFile version_path ReadMode
      l <- hGetLine vh
      hClose vh
      return $ unwords $ words l
    give_up :: IOException -> IO String
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
    give_up :: IOException -> IO Int
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

type Command = [String] -> CommandState -> ELIO ()

data CommandRecord = CommandRecord {
  cr_name :: String,
  cr_argdesc :: String,
  cr_command :: Command,
  cr_desc :: String }

commandList :: [CommandRecord]
commandList = [
      CommandRecord "help" "" helpCommand "this help",
      CommandRecord "quit" "" quitCommand "quit imcs",
      CommandRecord "me" "<player> <password>" meCommand "log in",
      CommandRecord "register" "<player> <password>" registerCommand
        "register a new name and log in",
      CommandRecord "password" "<password>" passwordCommand "change password",
      CommandRecord "list" "" listCommand "list available games",
      CommandRecord "ratings" "" ratingsCommand 
        "list player ratings (top 10 plus own)",
      CommandRecord "offer" "[<color>] [<time> [<opp-time>]]" offerCommand
        "offer a game with given constraints",
      CommandRecord "accept" "<game-id> [<my-color>]" acceptCommand
        "accept a game with an opponent",
      CommandRecord "clean" "" cleanCommand 
        "cancel all my outstanding offers", 
      CommandRecord "rerate" "" rerateCommand
        "reset my rating to the starting default" ]

adminCommandList :: [CommandRecord]
adminCommandList = commandList ++ [
      CommandRecord "stop" "" stopCommand 
        "stop the server and wait for in-process games" ]

cmd_lookup :: String -> [CommandRecord] -> Maybe CommandRecord
cmd_lookup cmd crs = find ((cmd ==) . cr_name) crs

sPutLn :: CommandState -> String -> ELIO ()
sPutLn cs s = sPutStrLn (cs_h cs) s

usage_str :: CommandRecord -> String
usage_str (CommandRecord {cr_name = name, cr_argdesc = argdesc}) =
  if argdesc == "" then name  else name ++ " " ++ argdesc

usage :: String -> CommandState -> ELIO ()
usage cmd cs =
  case cmd_lookup cmd adminCommandList of
    Nothing -> 
      error "internal error: usage on non-command"
    Just cr ->
      sPutLn cs $ printf "409 %s: usage: %s" cmd (usage_str cr)

putState :: CommandState -> ServiceState -> ELIO ()
putState cs ss = liftIO $ putMVar (cs_state cs) ss

takeState :: CommandState -> ELIO ServiceState
takeState cs = liftIO $ takeMVar (cs_state cs)

readState :: CommandState -> ELIO ServiceState
readState cs = liftIO $ readMVar (cs_state cs)

readMe :: CommandState -> ELIO (Maybe String)
readMe cs = liftIO $ readIORef $ cs_me cs

shClose :: CommandState -> ELIO ()
shClose cs = liftIO $ hClose $ cs_h cs

helpCommand :: Command
helpCommand [] cs = do
  maybe_my_name <- readMe cs
  sPutLn cs $  "210 imcs " ++ version ++ " help"
  let crs = 
        case maybe_my_name of
          Just "admin" -> adminCommandList
          _ -> commandList
  mapM_ put_cr crs
  sPutLn cs "."
  where
    put_cr cr =
      sPutLn cs $ printf " %s: %s" (usage_str cr) (cr_desc cr)
helpCommand _ cs = usage "help" cs

quitCommand :: Command
quitCommand [] cs = do
  logMsg $ "client " ++ cs_client_id cs ++ " quits"
  sPutLn cs "200 Goodbye"
  shClose cs
  finish
quitCommand _ cs = usage "quit" cs

meCommand :: Command
meCommand [name, password] cs = do
  ss <- readState cs
  case pw_lookup ss name of
    Nothing ->
        sPutLn cs "400 no such username: please use register command"
    Just (password', _) ->
        if password /= password'
        then
          sPutLn cs "401 wrong password"
        else do
          logMsg $ "client " ++ cs_client_id cs ++ " aka " ++ name
          liftIO $ writeIORef (cs_me cs) $ Just name
          sPutLn cs $ "201 hello " ++ name
meCommand _ cs = usage "me" cs

registerCommand :: Command
registerCommand [name, password] cs 
  | any (\c -> not (isAlphaNum c) && not (c `elem` "._-")) name =
      sPutLn cs "410 illegal characters in username"
  | any (\c -> not (isPrint c) || isSpace c) password = 
      sPutLn cs "411 illegal characters in password"
  | otherwise = do
      ss <- takeState cs
      case pw_lookup ss name of
        Just _ -> do
          putState cs ss
          sPutLn cs $ "402 username already exists: " ++
                      "please use password command to change"
        Nothing -> do
          let ServiceState game_id game_list pwf = ss
          let pwfe = PWFEntry name password baseRating
          let pwf' = pwf ++ [pwfe]
          let ss' = ServiceState game_id game_list pwf'
          liftIO $ write_pwf pwf' --- XXX failure will hang server
          putState cs ss'
          liftIO $ writeIORef (cs_me cs) $ Just name
          sPutLn cs $ "202 hello new user " ++ name
          logMsg $ "registered client " ++ cs_client_id cs ++
                   " as user " ++ name
registerCommand _ cs = usage "register" cs

passwordCommand :: Command
passwordCommand [password] cs = do
  maybe_my_name <- readMe cs
  case maybe_my_name of
    Nothing ->
      sPutLn cs "403 please use the me command first"
    Just my_name -> do
      ss <- takeState cs
      case pw_lookup ss my_name of
        Nothing -> do
          putState cs ss
          logMsg $ "client " ++ cs_client_id cs ++ " name " ++
                   my_name ++ "not in password file"
          sPutLn cs "500 you do not exist: go away"
        Just _ -> do
          let ServiceState game_id game_list pwf = ss
          let newpass e@(PWFEntry n _ rating)
               | n == my_name = PWFEntry n password rating
               | otherwise = e
          let pwf' = map newpass pwf
          let ss' = ServiceState game_id game_list pwf'
          liftIO $ write_pwf pwf'  --- XXX failure will hang server
          putState cs ss'
          sPutLn cs $ "203 password change for user " ++ my_name
          logMsg $ "changed password for client " ++ cs_client_id cs ++
                   " user " ++ my_name
passwordCommand _ cs = usage "password" cs

show_time :: Int -> String
show_time t = 
  printf "%d:%02d" (t `div` 60000) ((t `div` 1000) `mod` 60)

listCommand :: Command
listCommand [] cs = do
  ss@(ServiceState _ game_list _) <- readState cs
  sPutLn cs $ printf "211 %d available games" (length game_list)
  let lookup_rating other_name =
          case pw_lookup ss other_name of
            Nothing -> "?"
            Just (_, r) -> show r
  let output_game (GameResv game_id other_name _ color (t1, t2) _) =
          sPutLn cs $ " " ++ show game_id ++
                      " " ++ other_name ++
                      " " ++ [color] ++
                      " " ++ show_time t1 ++
                      " " ++ show_time t2 ++
                      " " ++ lookup_rating other_name ++
                      " [offer]"
      output_game (InProgress game_id name_white name_black _) =
          sPutLn cs $ " " ++ show game_id ++
                      " " ++ name_white ++
                      " " ++ name_black ++
                      " (" ++ lookup_rating name_white ++ "/" ++
                              lookup_rating name_black ++ ") " ++
                      " [in-progress]"
  mapM_ output_game game_list
  sPutLn cs "."
listCommand _ cs = usage "list" cs

ratingsCommand :: Command
ratingsCommand [] cs = do
  ServiceState _ _ pwf <- readState cs
  maybe_my_name <- readMe cs
  let top10 = 
        take 10 $ sortBy (comparing (negate . pwf_entry_rating)) pwf
  let rating_list =
        case maybe_my_name of
          Nothing -> top10
          Just my_name -> 
            case lookup_me my_name top10 of
              Just _ -> top10
              Nothing -> 
                case lookup_me my_name pwf of
                  Just pwe ->  top10 ++ [pwe]
                  Nothing -> error $ "internal error: no rating " ++ 
                                     "for logged-in player"
  sPutLn cs "212 ratings"
  mapM_ output_rating rating_list
  sPutLn cs "."
  where
    lookup_me my_name pwf =
      find ((my_name ==) . pwf_entry_name) pwf
    output_rating pwe =
      sPutLn cs $ " " ++ pwf_entry_name pwe ++ 
                  " " ++ show (pwf_entry_rating pwe)
ratingsCommand _ cs = usage "ratings" cs

check_color :: Maybe String -> Maybe String
check_color Nothing = Just "?"
check_color (Just "W") = Just "W"
check_color (Just "B") = Just "B"
check_color (Just "w") = Just "W"
check_color (Just "b") = Just "B"
check_color (Just "?") = Just "?"
check_color (Just _) = Nothing

offerCommand' :: Maybe String -> Maybe (Int, Int) -> 
                 CommandState -> ELIO ()
offerCommand' opt_color opt_times cs = do
  case check_color opt_color of
    Nothing -> 
      sPutLn cs $ "405 bad color " ++ fromJust opt_color
    Just my_color -> do
      maybe_my_name <- readMe cs
      case maybe_my_name of
        Nothing ->
          sPutLn cs "404 must set name first using me command"
        Just my_name -> do
          logMsg $ "client " ++ cs_client_id cs ++
                   " offers game as " ++ my_color
          wakeup <- liftIO $ newChan
          ss <- takeState cs
          let ServiceState game_id game_list pwf = ss
          let new_game =
                GameResv game_id my_name (cs_client_id cs) 
                         (head my_color) tc wakeup
          let service_state' =
                ServiceState (game_id + 1) (game_list ++ [new_game]) pwf
          liftIO $ write_game_id (game_id + 1)   --- XXX failure hangs server
          putState cs service_state'
          sPutLn cs $ "103 " ++ show game_id ++
                      " game waiting for offer acceptance"
          w <- liftIO $ readChan wakeup
          case w of
            Wakeup other_name other_id other_h other_color -> do
              let result_code = 
                    case other_color of
                      "W" -> "106 B "
                      "B" -> "105 W "
                      _ -> error "internal error: bad offer accept color"
              sPutLn cs $ result_code ++ show_time my_time ++ 
                          " " ++ show_time other_time ++ 
                " game starts"
              let my_info = ((cs_h cs, Just my_time),
                             my_name, cs_client_id cs, my_color)
              let other_info = ((other_h, Just other_time),
                                other_name, other_id, other_color)
              let ((p1, p1_name, p1_id), (p2, p2_name, p2_id)) =
                    sort_colors my_info other_info
              wchan <- liftIO $ newEmptyMVar
              let ip = InProgress game_id p1_name p2_name wchan
              ServiceState game_id'' game_list'' pwf'' <- takeState cs
              let gl' = game_list'' ++ [ip]
              putState cs $ ServiceState game_id'' gl' pwf''
              let run_game = do
                  let game_desc = show game_id ++ ": " ++
                                    p1_name ++ "(" ++ p1_id ++ ", W) vs " ++
                                    p2_name ++ "(" ++ p2_id ++ ")"
                  logMsg $ "game " ++ game_desc ++ " begins"
                  let path = log_path </> show game_id
                  game_log <- liftIO $ openFile path WriteMode
                  score <- liftIO $ withLogDo game_log $ do
                    time <- liftIO $ getClockTime
                    let date = calendarTimeToString $ toUTCTime time
                    logMsg game_desc
                    logMsg date
                    s <- doGame p1 p2
                    return s
                  logMsg $ "game " ++ game_desc ++ " ends"
                  when (p1_name /= p2_name) $ do
                    logMsg $ "updating ratings for " ++ p1_name ++ 
                             ", " ++ p2_name
                    p1_rating <- lookup_rating p1_name
                    p2_rating <- lookup_rating p2_name
                    update_rating p1_name p1_rating p2_rating score
                    update_rating p2_name p2_rating p1_rating (-score)
                  logMsg $ "client " ++ cs_client_id cs ++ " closes"
                  where
                    lookup_rating name = do
                      ss' <- liftIO $ readMVar $ cs_state cs
                      case pw_lookup ss' name of
                        Just (_, rating) -> return rating
                        --- XXX should never happen
                        Nothing -> return baseRating
                    update_rating name ra rb s = do
                      let ra' = updateRating ra rb s
                      ss' <- liftIO $ takeMVar $ cs_state cs
                      let ServiceState game_id''' game_list''' pwf''' = ss'
                      let newpass e@(PWFEntry n password _)
                            | n == name = PWFEntry n password ra'
                            | otherwise = e
                      let pwf' = map newpass pwf'''
                      let ss'' = ServiceState game_id''' game_list''' pwf'
                      liftIO $ write_pwf pwf'   --- XXX failure hangs server
                      liftIO $ putMVar (cs_state cs) ss''
              let clean_up e = do
                  logMsg $ "game " ++ cs_client_id cs ++
                    " incurs IO error: " ++ show e
                  liftIO $ close_it (cs_h cs)
                  liftIO $ close_it other_h
                  where
                    close_it :: Handle -> IO ()
                    close_it h' = 
                      catch (do
                                sPutStrNL h'
                                  "X fatal IO error: exiting"
                                hClose h')
                            ((\_ -> return ()) :: IOException -> IO ())
              lift $ catchLogIO run_game clean_up
              ServiceState game_id''' game_list''' pwf''' <- takeState cs
              let exclude_me (InProgress game_id' _ _ _)
                    | game_id == game_id' = False
                  exclude_me _ = True
              let gl''' = filter exclude_me game_list'''
              liftIO $ putMVar (cs_state cs) $ 
                ServiceState game_id''' gl''' pwf'''
              liftIO $ putMVar wchan ()
              finish
            Nevermind ->
              alsoLogMsg (cs_h cs) "421 offer countermanded"
      where
        sort_colors (player_l, name_l, id_l, color_l)
                    (player_r, name_r, id_r, color_r) =
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
              left = ((player_l, name_l, id_l), (player_r, name_r, id_r))
              right = ((player_r, name_r, id_r), (player_l, name_l, id_l))
  where
    tc@(my_time, other_time) =
      case opt_times of
        Just ts -> ts
        Nothing -> (default_time, default_time)


offerCommand :: Command
offerCommand args cs = do
  let (opt_color, rest) = parse_color args
  let maybe_times = rest >>= parse_times
  case maybe_times of
    Just opt_times -> offerCommand' opt_color opt_times cs
    _ -> usage "offer" cs
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

acceptCommand' ::  String -> String -> CommandState -> ELIO ()
acceptCommand' accept_game_id my_color cs = do
  maybe_my_name <- readMe cs
  case (parse_int accept_game_id, maybe_my_name) of
    (_, Nothing) ->
      sPutLn cs "406 must set name first using me command"
    (Nothing, _) ->
      sPutLn cs "407 bad game id"
    (Just ask_id, Just my_name) -> do
      ss@(ServiceState game_id game_list pwf) <- takeState cs
      game <- game_lookup game_list ask_id
      case game of
        Left err -> do
          putState cs ss
          alsoLogMsg (cs_h cs) err
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
              putState cs $ ServiceState game_id game_list' pwf
              logMsg $ "client " ++ cs_client_id cs ++
                       " accepts " ++ show other_name
              sPutLn cs $ 
                code ++ " " ++ my_color' ++ 
                " " ++ show_time my_time ++ " " ++
                show_time other_time ++ " game starts"
              liftIO $ writeChan wakeup $ 
                Wakeup my_name (cs_client_id cs) (cs_h cs) my_color'
              finish
            Nothing -> do
              putState cs ss
              alsoLogMsg (cs_h cs) $ "405 bad color " ++ my_color
        _ -> error "internal error: bad game lookup"

acceptCommand :: Command
acceptCommand [game_id] cs =
  acceptCommand' game_id "?" cs
acceptCommand [game_id, color] cs
  | isJust ccolor = acceptCommand' game_id (fromJust ccolor) cs
    where
      ccolor = check_color (Just color)
acceptCommand _ cs = usage "accept" cs  

cleanCommand :: Command
cleanCommand [] cs = do
  maybe_my_name <- readMe cs
  case maybe_my_name of
    Nothing -> sPutLn cs "406 must set name first using me command"
    Just my_name -> do
      ServiceState game_id game_list pwf <- takeState cs
      let (my_list, other_list) = partition my_game game_list
      let ss' = ServiceState game_id other_list pwf
      putState cs ss'
      liftIO $ mapM_ close_game my_list
      sPutLn cs $ "204 " ++ show (length my_list) ++
                  " games cleaned"
      where
        my_game (GameResv { game_resv_name = name })
            | name == my_name = True
        my_game _ = False
        close_game gr = writeChan (game_resv_wakeup gr) Nevermind
cleanCommand _ cs = usage "clean" cs

rerateCommand :: Command
rerateCommand [] cs = do
  maybe_my_name <- liftIO $ readIORef (cs_me cs)
  case maybe_my_name of
    Nothing ->
      sPutLn cs "403 please use the me command first"
    Just my_name -> do
      ss <- liftIO $ takeMVar (cs_state cs)
      case pw_lookup ss my_name of
        Nothing -> do
          liftIO $ putMVar (cs_state cs) ss
          logMsg $ "client " ++ cs_client_id cs ++ " name " ++
                   my_name ++ "not in password file"
          sPutLn cs "500 you do not exist: go away"
        Just _ -> do
          let ServiceState game_id game_list pwf = ss
          let newrating e@(PWFEntry n password _)
               | n == my_name = PWFEntry n password baseRating
               | otherwise = e
          let pwf' = map newrating pwf
          let ss' = ServiceState game_id game_list pwf'
          liftIO $ do
            write_pwf pwf'  --- XXX failure will hang server
            putMVar (cs_state cs) ss'
          sPutLn cs $ "206 rating reset for user " ++ my_name
          logMsg $ "reset rating for client " ++ cs_client_id cs ++
                   " user " ++ my_name
rerateCommand _ cs = usage "rerate" cs

stopCommand :: Command
stopCommand [] cs = do
  maybe_my_name <- readMe cs
  case maybe_my_name of
    Nothing -> sPutLn cs "406 must set name first using me command"
    Just "admin" -> do
      logMsg $ "stopping server for " ++ cs_client_id cs
      sPutLn cs "104 stopping server"
      ServiceState game_id game_list pwf <- takeState cs
      _ <- liftIO $ takeMVar (cs_reaccept cs)
      liftIO $ putMVar (cs_reaccept cs) False
      let ss' = ServiceState game_id [] pwf
      putState cs ss'
      liftIO $ mapM_ close_game game_list
      sPutLn cs "205 server stopped"
      liftIO $ hClose (cs_h cs)
      liftIO $ throwTo (cs_main_thread cs) ExitSuccess
      finish                                                       
      where
        close_game (GameResv { game_resv_wakeup = w }) =
            writeChan w Nevermind
        close_game (InProgress { in_progress_wakeup = w }) = do
            readMVar w
    Just _ -> sPutLn cs "502 admin only"
stopCommand _ cs = usage "stop" cs

doCommands :: (ThreadId, MVar Bool) -> (Handle, String)
           -> MVar ServiceState -> LogIO ()
doCommands (main_thread, reaccept) (h, client_id) state = do
  sPutStrLn h $ "100 imcs " ++ version
  me <- liftIO $ newIORef Nothing
  let params = CS main_thread reaccept h client_id state me
  _ <- runErrorT $ forever $ do
    line <- liftIO $ sGetLine h
    case words line of
      [] -> 
        return ()
      cmd : args ->
        case cmd_lookup cmd adminCommandList of
          Nothing -> sPutStrLn h $ "501 unknown command"
          Just cr -> cr_command cr args params
  return ()
