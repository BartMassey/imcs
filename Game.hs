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
  case unwords $ words $ move_str of
    "resign" -> return Resign
    move_str' ->
        case readMove move_str' of
          Nothing -> return InvalidMove
          Just mov -> case runST check_move of
                        True -> return $ GoodMove mov
                        False -> return $ IllegalMove
                      where
                        check_move = do
                          state <- animateProblem problem
                          candidates <- moves state
                          return (elem mov candidates)

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
    time_fmt Nothing = " -"

do_turn :: CState -> CState -> Problem -> LogIO (Maybe (Problem, Maybe Int))
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
    Just 0 -> do
      let loser = (showSide . problemToMove) problem
      alsoLogMsg other_h $ [loser] ++ " loses on time"
      return Nothing
    _ -> do
      case movt of
        Resign -> do
          let loser = (showSide . problemToMove) problem
          alsoLogMsg other_h $ [loser] ++ " resigns"
          return Nothing
        IllegalMove -> do
          alsoLogMsg this_h ("? illegal move")
          return (Just (problem, time'))
        InvalidMove -> do
          alsoLogMsg this_h ("? invalid move")
          return (Just (problem, time'))
        GoodMove mov -> do
          let (captured, stop, problem') = runST (execute_move mov)
          case stop of
            True -> do
              case captured of
                'K' -> report "B wins"
                'k' -> report "W wins"
                _   -> report "draw"
              return Nothing
              where
                report msg = do
                  alsoLogMsg this_h msg
                  liftIO $ hPutStrLn other_h msg
            False -> do
              let move_string = showMove mov
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

run_game :: Problem -> CState -> CState -> LogIO ()
run_game problem (h, t) other = do
  let side = problemToMove problem
  let trn = problemTurn problem
  let t0 = case (side, trn) of
             (White, 1) -> Nothing
             _ -> t
  result <- do_turn (h, t0) other problem
  case result of
    Nothing -> return ()
    Just (problem', t') -> do
        let side' = problemToMove problem'
        if side' == side 
          then run_game problem' (h, t') other
          else do
            let t1 = case (t, t0) of
                       (Just _, Nothing) -> t
                       _ -> t'
            run_game problem' other (h, t1)

doProblem :: Problem -> CState -> CState -> LogIO ()
doProblem problem w@(h_w, _) b@(h_b, _) = do
  case problemToMove problem of
    White -> run_game problem w b
    Black -> run_game problem b w
    _ -> error "internal error: run_game with no color"
  liftIO $ hClose h_w
  liftIO $ hClose h_b

doGame :: CState -> CState -> LogIO ()
doGame = doProblem startProblem         
