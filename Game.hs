--- Copyright © 2009 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "MIT License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

module Game(CState, doProblem, doGame) where

import Control.Concurrent.Timeout
import Control.Monad.ST
import System.IO
import System.Time
import Text.Printf

import Log
import Board
import SNewLine
import State

type CState = (Handle, Maybe Int)

data MoveResult = Timeout | Resign | InvalidMove String | IllegalMove String | GoodMove Move

read_move :: Problem -> (Handle, TimerState) -> IO MoveResult
read_move problem (handle, deadline) =
  (move_result . fmap words)  `fmap`
    timeout microsecs (sGetLine handle)
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

do_turn :: TCState -> TCState -> Problem -> LogIO TurnResult
do_turn (this_h, this_t) (other_h, other_t) problem = do
  alsoLogMsg this_h ""
  alsoLogMsg this_h (showProblem problem)
  alsoLogMsg this_h (times_fmt this_t other_t)
  then_msecs <- liftIO $ get_clock_time_ms
  movt <- liftIO $ read_move problem (this_h, this_t)
  now_msecs <- liftIO $ get_clock_time_ms
  let elapsed = fromIntegral (now_msecs - then_msecs)
  let time' = update_time this_t elapsed
  case time' of
    TimeRemaining 0 -> time_win
    _ -> do
      case movt of
        Timeout -> time_win
        Resign -> do
          let loser = (showSide . problemToMove) problem
          case loser of
            'B' -> do
              report "231" "W wins on resignation"
              return $ Right 1
            'W' -> do
              report "232" "B wins on resignation"
              return $ Right (-1)
            _ -> error "internal error: unknown color"
        IllegalMove s -> do
          alsoLogMsg this_h ("- illegal move " ++ s)
          return (Left (problem, time'))
        InvalidMove s -> do
          alsoLogMsg this_h ("- invalid move" ++ s)
          return (Left (problem, time'))
        GoodMove mov -> do
          let (captured, stop, problem') = runST (execute_move mov)
          let move_string = showMove mov
          logMsg move_string
          sPutStrLn other_h $ "! " ++ move_string
          case stop of
            True -> do
              case captured of
                'k' -> do
                  report "231" "W wins"
                  return $ Right 1
                'K' -> do
                  report "232" "B wins"
                  return $ Right (-1)
                _   -> do
                  report "230" "draw"
                  return $ Right 0
            False -> do
              return (Left (problem', start_clock time'))
  where
    time_win = do
      let loser = (showSide . problemToMove) problem
      case loser of
        'B' -> do
          report "231" "W wins on time"
          return $ Right 1
        'W' -> do
          report "232" "B wins on time"
          return $ Right (-1)
        _ -> error "internal error: unknown color"
    execute_move mov = do
      state <- animateProblem problem
      (state', undo) <- move state mov
      stop <- gameOver state' undo
      problem' <- snapshotState state'
      return (capture undo, stop, problem')
    report code msg = do
      sPutStrLn this_h $ "= " ++ msg
      sPutStrLn other_h $ "= " ++ msg
      sPutStrLn this_h $ code ++ " " ++ msg
      sPutStrLn other_h $ code ++ " " ++ msg
      logMsg $ code ++ " " ++ msg
      liftIO $ hClose this_h
      liftIO $ hClose other_h

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
