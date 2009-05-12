--- Copyright © 2009 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "MIT License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

module Game(CState, doProblem, doGame) where

import Data.Maybe
import Control.Monad
import Control.Monad.ST
import System.IO
import System.Time
import Text.Printf

import Log
import Board
import State

type CState = (Handle, Maybe Int)

data MoveResult = Resign | InvalidMove | IllegalMove | GoodMove Move

read_move :: Problem -> Handle -> IO MoveResult
read_move problem handle = do
  move_str <- hGetLine handle
  case words move_str of
    ["resign"] -> return Resign
    ["!", move_word] -> process_move move_word
    [move_word] -> process_move move_word
    _ -> return InvalidMove
    where
      process_move move_word = 
          case readMove move_word of
            Nothing -> return InvalidMove
            Just mov -> case runST check_move of
                          True -> return $ GoodMove mov
                          False -> return IllegalMove
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
  return (sec * 1000 + (picosec `div` (10^9)))

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

do_turn :: TCState -> TCState -> Problem -> LogIO (Maybe (Problem, TimerState))
do_turn (this_h, this_t) (other_h, other_t) problem = do
  alsoLogMsg this_h ""
  alsoLogMsg this_h (showProblem problem)
  alsoLogMsg this_h (times_fmt this_t other_t)
  then_msecs <- liftIO $ get_clock_time_ms
  movt <- liftIO $ read_move problem this_h 
  now_msecs <- liftIO $ get_clock_time_ms
  let elapsed = fromIntegral (now_msecs - then_msecs)
  let time' = update_time this_t elapsed
  case time' of
    TimeRemaining 0 -> do
      let loser = (showSide . problemToMove) problem
      case loser of
        'B' -> report "231" "W wins on time"
        'W' -> report "232" "B wins on time"
      return Nothing
    _ -> do
      case movt of
        Resign -> do
          let loser = (showSide . problemToMove) problem
	  case loser of
            'B' -> report "231" "W wins on resignation"
            'W' -> report "232" "B wins on resignation"
          return Nothing
        IllegalMove -> do
          alsoLogMsg this_h ("- illegal move")
          return (Just (problem, time'))
        InvalidMove -> do
          alsoLogMsg this_h ("- invalid move")
          return (Just (problem, time'))
        GoodMove mov -> do
          let (captured, stop, problem') = runST (execute_move mov)
          case stop of
            True -> do
              case captured of
                'K' -> report "232" "B wins"
                'k' -> report "231" "W wins"
                _   -> report "230" "draw"
              return Nothing
            False -> do
              let move_string = showMove mov
              logMsg move_string
              liftIO $ hPutStrLn other_h $ "! " ++ move_string
              return (Just (problem', start_clock time'))
  where
    execute_move mov = do
      state <- animateProblem problem
      (state', undo) <- move state mov
      stop <- gameOver state' undo
      problem' <- snapshotState state'
      return (capture undo, stop, problem')
    report code msg = do
      liftIO $ hPutStrLn this_h $ "= " ++ msg
      liftIO $ hPutStrLn other_h $ "= " ++ msg
      alsoLogMsg this_h $ code ++ " " ++ msg
      liftIO $ hPutStrLn other_h $ code ++ " " ++ msg

run_game :: Problem -> TCState -> TCState -> LogIO ()
run_game problem (h, t) other = do
  let side = problemToMove problem
  let trn = problemTurn problem
  result <- do_turn (h, t) other problem
  case result of
    Nothing -> return ()
    Just (problem', t') -> do
        let side' = problemToMove problem'
        if side' == side 
          then run_game problem' (h, t') other
          else do
            run_game problem' other (h, t')

doProblem :: Problem -> CState -> CState -> LogIO ()
doProblem problem w b = do
  case problemToMove problem of
    White -> run_game problem (stop_clock w) (pend_clock b)
    Black -> run_game problem (stop_clock b) (pend_clock w)
    _ -> error "internal error: run_game with no color"
  close_h w
  close_h b
  where
    close_h (h, _) = liftIO $ hClose h
    stop_clock (h, Nothing) = (h, Untimed)
    stop_clock (h, Just t) = (h, FirstMove t)
    pend_clock (h, Nothing) = (h, Untimed)
    pend_clock (h, Just t) = (h, TimeRemaining t)

doGame :: CState -> CState -> LogIO ()
doGame = doProblem startProblem         
