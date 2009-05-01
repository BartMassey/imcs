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
import Network (withSocketsDo)

import System.Console.ParseArgs

import Board
import State
import Connection
import Game

data Option = OptionPort
              deriving (Ord, Eq, Show)

argd :: [ Arg Option ]
argd = [ Arg { argIndex = OptionPort,
               argName = Just "port",
               argAbbr = Just 'p',
               argData = argDataDefaulted "port" ArgtypeInt 3589,
               argDesc = "Server port" } ]

run_game :: Problem -> CState -> CState -> IO ()
run_game problem this@(h, t) other = do
  result <- doTurn this other problem
  case result of
    Nothing -> return ()
    Just (problem', t') -> do
        let side = problemToMove problem
        let side' = problemToMove problem'
        if side' == side 
          then run_game problem' this other
          else run_game problem' other (h, t')

one_game :: Maybe Int -> (Int, Int) -> IO ()
one_game time (port_w, port_b) = do
  handle_w <- connectionInit port_w 'W'
  handle_b <- connectionInit port_b 'B'
  case problemToMove startProblem of
    White -> run_game startProblem (handle_w, time) (handle_b, time)
    Black -> run_game startProblem (handle_w, time) (handle_b, time)
  hClose handle_w
  hClose handle_b

real_main :: Int -> IO ()
real_main port = do
  master <- masterInit port
  forever $ do
    client <- masterAccept master
    hClose client

main :: IO ()
main = do
  args <- parseArgsIO ArgsComplete argd
  let port = fromJust (getArgInt args OptionPort)
  withSocketsDo (real_main port)
