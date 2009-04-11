--- Copyright © 2009 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "MIT License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

module Connection(CState, connectionInit, doTurn) where

import Control.Monad
import Control.Monad.ST
import Network
import Network.Socket as NS
import System.IO
import System.Time
import Data.Bits
import Text.Printf

import Board
import State

type CState = (Handle, Maybe Int)

connectionInit :: Int -> Side -> IO Handle
connectionInit port_num side = do
  hPutStrLn stderr ("listening for " ++ [showSide side] ++
                    " on port " ++ show port_num)
  let port_num' = rotate (fromIntegral port_num) 8
  let port = (PortNumber . NS.PortNum) port_num'
  listen_socket <- listenOn port
  (handle, hostname, _) <- Network.accept listen_socket
  hPutStrLn stderr ("got " ++ [showSide side] ++
                    " host " ++ hostname ++
                    " on port " ++ show port_num)
  hPutStrLn handle [showSide side]
  hSetBuffering handle LineBuffering
  return handle

chop :: String -> String
chop = dropWhile ((flip elem) [' ', '\t', '\r', '\n'])

chomp :: String -> String
chomp = reverse . chop . reverse . chop

read_move :: Handle -> IO Move
read_move handle = do
  move_str <- hGetLine handle
  case readMove (chomp move_str) of
    Just move -> return move
    Nothing -> error ("ill-formatted move string '" ++ move_str ++ "'")

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
  case this_t of
    Just t -> do
      hPutStr h " "
      time_fmt t
  case other_t of
    Just t -> do
      hPutStr h " "
      time_fmt t
  hPutStrLn h ""
  where
    time_fmt t = do
        let mins = t `div` 60000
        let secs = fromIntegral (t - 60000 * mins) / 1000 :: Double
        hPrintf h "%02d:%06.3f" mins secs


doTurn :: CState -> CState -> Problem -> IO (Maybe (Problem, Maybe Int))
doTurn (this_h, this_t) (other_h, other_t) problem = do
  hPutStrLn this_h ""
  hPutStr this_h (showProblem problem)
  hPutStrLn stderr ""
  hPutStr stderr (showProblem problem)
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
      hPutStrLn stderr ([ loser ] ++ " loses on time")
      return Nothing
    _ -> do
      let () = runST (check_move mov)
      let (captured, stop, problem') = runST (execute_move mov)
      case stop of
        True -> do
          case captured of
            'K' -> hPutStrLn stderr "B wins"
            'k' -> hPutStrLn stderr "W wins"
            _   -> hPutStrLn stderr "draw"
          return Nothing
        False -> do
          hPutStrLn stderr (showMove mov)
          hPutStrLn other_h ("! " ++ showMove mov)
          return (Just (problem', time'))
  where
    execute_move mov = do
      state <- animateProblem problem
      (state', undo) <- move state mov
      stop <- gameOver state' undo
      problem' <- snapshotState state'
      return (capture undo, stop, problem')
    check_move mov = do
      state <- animateProblem problem
      candidates <- moves state
      unless (elem mov candidates)
             (error ("illegal move" ++ showMove mov))
      return ()    
