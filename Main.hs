--- Copyright © 2009 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "MIT License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

module Main
where

import Data.Maybe
import Control.Monad
import Control.Monad.ST
import System.IO
import Network
import ParseArgs

import Board
import State
import Connection

data Option = OptionOffset
            | OptionPosition
            | OptionTime
              deriving (Ord, Eq, Show)

argd :: [ Arg Option ]
argd = [ Arg { argIndex = OptionPosition,
               argName = Just "position",
               argAbbr = Nothing,
               argData = argDataOptional "position-file" ArgtypeString,
               argDesc = "File containing initial position" },
         Arg { argIndex = OptionTime,
               argName = Just "time",
               argAbbr = Just 't',
               argData = argDataOptional "secs" ArgtypeInt,
               argDesc = "Seconds per side for 40 moves" },
         Arg { argIndex = OptionOffset,
               argName = Nothing,
               argAbbr = Nothing,
               argData = argDataDefaulted "offset" ArgtypeInt 0,
               argDesc = "Port offset" } ]

run_game :: Problem -> CState -> CState -> IO ()
run_game problem this@(h, t) other = do
  result <- doTurn this other problem
  case result of
    Nothing -> return ()
    Just (problem', t') -> run_game problem' other (h, t')

real_main :: IO ()
real_main = do
  args <- parseArgsIO ArgsComplete argd
  let offset = fromJust (getArgInt args OptionOffset)
  let time = getArgInt args OptionTime
  mfh <- getArgFile args OptionPosition ReadMode
  let time' = case time of
                Just t -> Just (1000 * t)
  handle_w <- connectionInit (3589 + offset) White
  handle_b <- connectionInit (3590 + offset) Black
  problem <- case mfh  of
               Nothing -> return startProblem
               Just pf -> do
                 ps <- hGetContents pf
                 return (readProblem ps)
  case problemToMove problem of
    White -> run_game problem (handle_w, time') (handle_b, time')
    Black -> run_game problem (handle_w, time') (handle_b, time')
  hClose handle_w
  hClose handle_b

main :: IO ()
main = withSocketsDo real_main
