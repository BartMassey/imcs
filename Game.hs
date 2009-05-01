--- Copyright © 2009 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "MIT License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

module Game(CState, doGame) where

import Data.Maybe
import Control.Monad
import Control.Monad.ST
import System.IO
import System.Time
import Text.Printf

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

show_times :: Handle -> Maybe Int -> Maybe Int -> IO ()
show_times h this_t other_t = do
  hPutStr h "?"
  show_time this_t
  show_time other_t
  hPutStrLn h ""
  where
    time_fmt t = do
        let mins = t `div` 60000
        let secs = fromIntegral (t - 60000 * mins) / 1000 :: Double
        hPrintf h "%02d:%06.3f" mins secs
    show_time (Just t) = do
      hPutStr h " "
      time_fmt t
    show_time Nothing = return ()


do_turn :: CState -> CState -> Problem -> IO (Maybe (Problem, Maybe Int))
do_turn (this_h, this_t) (other_h, other_t) problem = do
  hPutStrLn this_h ""
  hPutStr this_h (showProblem problem)
  putStrLn ""
  putStr (showProblem problem)
  show_times this_h this_t other_t
  show_times stderr this_t other_t
  then_msecs <- get_clock_time_ms
  mov <- read_move this_h 
  now_msecs <- get_clock_time_ms
  let elapsed = fromIntegral (now_msecs - then_msecs)
  let time' = update_time this_t elapsed
  case time' of
    Just 0 -> do
      let loser = (showSide . problemToMove) problem
      putStrLn ([ loser ] ++ " loses on time")
      return Nothing
    _ -> do
      let ok = runST (check_move mov)
      case ok of
        False -> do
          putStrLn ("? illegal move")
          hPutStrLn this_h ("? illegal move")
          return (Just (problem, time'))
        True -> do
          let (captured, stop, problem') =
                runST (execute_move (fromJust mov))
          case stop of
            True -> do
              case captured of
                'K' -> putStrLn "B wins"
                'k' -> putStrLn "W wins"
                _   -> putStrLn "draw"
              return Nothing
            False -> do
              putStrLn (showMove (fromJust mov))
              hPutStrLn other_h ("! " ++ showMove (fromJust mov))
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

run_game :: Problem -> CState -> CState -> IO ()
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

doGame :: Maybe Int -> (Handle, Handle) -> IO ()
doGame time (handle_w, handle_b) = do
  case problemToMove startProblem of
    White -> run_game startProblem (handle_w, time) (handle_b, time)
    Black -> run_game startProblem (handle_w, time) (handle_b, time)
  hClose handle_w
  hClose handle_b
