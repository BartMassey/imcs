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
import Readline
import Board
import State

type CState = (Handle, Maybe Int)

read_move :: Handle -> IO (Maybe Move)
read_move handle = do
  move_str <- hReadline handle
  return (readMove move_str)

update_time :: Maybe Int -> Int -> Maybe Int
update_time Nothing _ = Nothing
update_time (Just had) lost = Just ((had - lost) `max` 0)

get_clock_time_ms :: IO Integer
get_clock_time_ms = do
  TOD sec picosec <- getClockTime
  return (sec * 1000 + (picosec `div` (10^9)))

times_fmt :: Maybe Int -> Maybe Int -> String
times_fmt this_t other_t =
  "?" ++ time_fmt this_t ++ time_fmt other_t
  where
    time_fmt (Just t) = printf " %02d:%06.3f" mins secs where
        mins = t `div` 60000
        secs = fromIntegral (t - 60000 * mins) / 1000 :: Double
    time_fmt Nothing = " "

do_turn :: CState -> CState -> Problem -> LogIO (Maybe (Problem, Maybe Int))
do_turn (this_h, this_t) (other_h, other_t) problem = do
  alsoLogMsg this_h ""
  alsoLogMsg this_h (showProblem problem)
  alsoLogMsg this_h (times_fmt this_t other_t)
  then_msecs <- liftIO $ get_clock_time_ms
  mov <- liftIO $ read_move this_h 
  now_msecs <- liftIO $ get_clock_time_ms
  let elapsed = fromIntegral (now_msecs - then_msecs)
  let time' = update_time this_t elapsed
  case time' of
    Just 0 -> do
      let loser = (showSide . problemToMove) problem
      logMsg $ [loser] ++ " loses on time"
      return Nothing
    _ -> do
      let ok = runST (check_move mov)
      case ok of
        False -> do
          alsoLogMsg this_h ("? illegal move")
          return (Just (problem, time'))
        True -> do
          let (captured, stop, problem') =
                runST (execute_move (fromJust mov))
          case stop of
            True -> do
              case captured of
                'K' -> logMsg "B wins"
                'k' -> logMsg "W wins"
                _   -> logMsg "draw"
              return Nothing
            False -> do
              let move_string = showMove (fromJust mov)
              logMsg move_string
              liftIO $ hPutStrLn other_h $ "! " ++ move_string
              return (Just (problem', time'))
  where
    execute_move mov = do
      state <- animateProblem problem
      (state', undo) <- move state mov
      stop <- gameOver state' undo
      problem' <- snapshotState state'
      return (capture undo, stop, problem')
    check_move (Just mov) = do
      state <- animateProblem problem
      candidates <- moves state
      return (elem mov candidates)
    check_move Nothing = return False

run_game :: Problem -> CState -> CState -> LogIO ()
run_game problem this@(h, t) other = do
  result <- do_turn this other problem
  case result of
    Nothing -> return ()
    Just (problem', t') -> do
        let side = problemToMove problem
        let side' = problemToMove problem'
        if side' == side 
          then run_game problem' this other
          else run_game problem' other (h, t')

doProblem :: Problem -> CState -> CState -> LogIO ()
doProblem problem w@(h_w, _) b@(h_b, _) = do
  case problemToMove problem of
    White -> run_game problem w b
    Black -> run_game problem b w
  liftIO $ hClose h_w
  liftIO $ hClose h_b

doGame :: CState -> CState -> LogIO ()
doGame = doProblem startProblem         
