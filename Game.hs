{-# LANGUAGE CPP #-}
--- Copyright © 2009 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "MIT License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

module Game(CState, doProblem, doGame) where

#if __GLASGOW_HASKELL__ < 706
import Prelude hiding (catch)
#endif
import Control.Concurrent.Timeout
import Control.Exception.Base (catch)
import Control.Monad.ST
import Data.Char
import System.IO
#if __GLASGOW_HASKELL__ < 706
import System.IO.Error hiding (catch)
#endif
import System.Time
import Text.Printf

import Log
import Board
import SNewLine
import State

type CState = (Handle, Maybe Int)

data MoveResult = Timeout | Resign | InvalidMove String 
                | IllegalMove String | GoodMove Move | Disconnect

sendToClient :: LogIO () -> String -> Handle -> LogIO ()
sendToClient action desc other_h =
  catchLogIO 
    action
    (\e -> do
        alsoLogMsg other_h $ desc ++ " failed: " ++ show e
        liftIO $ ioError $ 
          userError $ "connection failed before/during " ++ desc)

read_move :: Problem -> (Handle, TimerState) -> IO MoveResult
read_move problem (handle, deadline) =
  (catch :: IO MoveResult -> (IOError -> IO MoveResult) -> IO MoveResult)
    ((move_result . fmap words) `fmap` timeout microsecs (sGetLine handle))
    (\_ -> return Disconnect)
  where
    microsecs =
      case deadline of
        Untimed -> -1
        FirstMove _ -> mktime 60000
        TimeRemaining t -> mktime (t + 10000)
        where
          mktime :: Int -> Integer
          mktime t = 1000 * fromIntegral t
    move_result Nothing = Timeout
    move_result (Just ["resign"]) = Resign
    move_result (Just ["!", move_word]) = process_move move_word
    move_result (Just [move_word]) = process_move move_word
    move_result (Just ss) = InvalidMove $ unwords ss
    process_move move_word = 
      case readMove move_word of
          Nothing -> InvalidMove move_word
          Just mov -> case runST check_move of
                        True -> GoodMove mov
                        False -> IllegalMove move_word
                      where
                        check_move = do
                          state <- animateProblem problem
                          candidates <- moves state
                          return (elem mov candidates)

data TimerState = Untimed | FirstMove Int | TimeRemaining Int

update_time :: TimerState -> Int -> TimerState
update_time Untimed _ = Untimed
update_time fm@(FirstMove _) _ = fm
update_time (TimeRemaining had) lost = TimeRemaining ((had - lost) `max` 0)

start_clock :: TimerState -> TimerState
start_clock Untimed = Untimed
start_clock (FirstMove t) = TimeRemaining t
start_clock tr@(TimeRemaining _) = tr

get_clock_time_ms :: IO Integer
get_clock_time_ms = do
  TOD sec picosec <- getClockTime
  return (sec * 1000 + (picosec `div` (10^(9::Integer))))

times_fmt :: TimerState -> TimerState -> String
times_fmt this_t other_t =
  "?" ++ time_fmt this_t ++ time_fmt other_t
  where
    time_fmt (TimeRemaining t) = printf " %02d:%06.3f" mins secs where
        mins = t `div` 60000
        secs = fromIntegral (t - 60000 * mins) / 1000 :: Double
    time_fmt (FirstMove t) = time_fmt $ TimeRemaining t
    time_fmt Untimed = " -"

type TCState = (Handle, TimerState)
type TurnResult = Either (Problem, TimerState) Int

sanitize_string :: String -> String
sanitize_string s = 
  concatMap sanitize_char s
  where
    sanitize_char '\\' = "\\\\"
    sanitize_char c
      | isPrint c = [c]
      | otherwise = printf "\\[%02x]" $ ord c

do_turn :: TCState -> TCState -> Problem -> LogIO TurnResult
do_turn (this_h, this_t) (other_h, other_t) problem = do
  sendToClient
    (do
        alsoLogMsg this_h ""
        alsoLogMsg this_h (showProblem problem)
        alsoLogMsg this_h (times_fmt this_t other_t))
    (this_side ++ " send move request")
    other_h
  then_msecs <- liftIO $ get_clock_time_ms
  movt <- liftIO $ read_move problem (this_h, this_t)
  now_msecs <- liftIO $ get_clock_time_ms
  let elapsed = fromIntegral (now_msecs - then_msecs)
  let time' = update_time this_t elapsed
  case time' of
    TimeRemaining 0 -> free_win "time"
    _ -> do
      case movt of
        Timeout -> free_win "time"
        Disconnect -> free_win "disconnect"
        Resign -> do
          let loser = (showSide . problemToMove) problem
          case loser of
            'B' -> do
              report problem "231" "W wins on opponent resignation"
              return $ Right 1
            'W' -> do
              report problem "232" "B wins on opponent resignation"
              return $ Right (-1)
            _ -> error "internal error: unknown color"
        IllegalMove s -> do
          sendToClient
            (alsoLogMsg this_h ("- illegal move " ++ s))
            (this_side ++ " send illegal move notice")
            other_h
          return (Left (problem, time'))
        InvalidMove s -> do
          sendToClient
            (alsoLogMsg this_h ("- invalid move " ++ sanitize_string s))
            (this_side ++ " send invalid move notice")
            other_h
          return (Left (problem, time'))
        GoodMove mov -> do
          let (captured, stop, problem') = runST (execute_move mov)
          let move_string = showMove mov
          logMsg move_string
          let what = other_side ++ " move string send"
          sendToClient (sPutStrLn other_h $ "! " ++ move_string) what this_h
          case stop of
            True -> do
              case captured of
                'k' -> do
                  report problem' "231" "W wins"
                  return $ Right 1
                'K' -> do
                  report problem' "232" "B wins"
                  return $ Right (-1)
                _   -> do
                  report problem' "230" "draw"
                  return $ Right 0
            False -> do
              return (Left (problem', start_clock time'))
  where
    to_move = problemToMove problem
    this_side = [showSide to_move]
    other_side = [showSide $ opponent to_move]
    free_win cause = do
      let loser = (showSide . problemToMove) problem
      case loser of
        'B' -> do
          report problem "231" ("W wins on opponent " ++ cause)
          return $ Right 1
        'W' -> do
          report problem "232" ("B wins on opponent " ++ cause)
          return $ Right (-1)
        _ -> error "internal error: unknown color"
    execute_move mov = do
      state <- animateProblem problem
      (state', undo) <- move state mov
      stop <- gameOver state' undo
      problem' <- snapshotState state'
      return (capture undo, stop, problem')
    report pr code msg = do
      let result1 = "= " ++ msg
      let result2 = code ++ " " ++ msg
      logMsg ""
      logMsg $ showProblem pr
      logMsg result1
      safe_stc $ do
        stc result1 (this_side ++ " report result") this_h other_h
        stc result2 (this_side ++ " report result code") this_h other_h
      safe_stc $ do
        stc result1 (other_side ++ " report result") other_h this_h
        stc result2 (other_side ++ " report result code") other_h this_h
      liftIO $ hClose this_h
      liftIO $ hClose other_h
      where
        safe_stc action =
          catchLogIO action (\_ -> return ())
        stc m desc th oh =
          sendToClient (sPutStrLn th m) desc oh

run_game :: Problem -> TCState -> TCState -> LogIO Int
run_game problem (h, t) other = do
  let side = problemToMove problem
  result <- do_turn (h, t) other problem
  case result of
    Left (problem', t') -> do
      let side' = problemToMove problem'
      if side' == side 
         then run_game problem' (h, t') other
         else do
           run_game problem' other (h, t')
    Right s -> return s

doProblem :: Problem -> CState -> CState -> LogIO Int
doProblem problem w b = do
  s <- case problemToMove problem of
    White -> run_game problem (stop_clock w) (pend_clock b)
    Black -> run_game problem (stop_clock b) (pend_clock w)
    _ -> error "internal error: run_game with no color"
  close_h w
  close_h b
  return s
  where
    close_h (h, _) = liftIO $ hClose h
    stop_clock (h, Nothing) = (h, Untimed)
    stop_clock (h, Just t) = (h, FirstMove t)
    pend_clock (h, Nothing) = (h, Untimed)
    pend_clock (h, Just t) = (h, TimeRemaining t)

doGame :: CState -> CState -> LogIO Int
doGame = doProblem startProblem         
