--- Copyright © 2009 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "MIT License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

module Main
where

import Data.Maybe
import System.IO
import Control.Monad
import Network (withSocketsDo)

import System.Console.ParseArgs

import Connection
import Game
import Service

data Option = OptionPort
              deriving (Ord, Eq, Show)

argd :: [ Arg Option ]
argd = [ Arg { argIndex = OptionPort,
               argName = Just "port",
               argAbbr = Just 'p',
               argData = argDataDefaulted "port" ArgtypeInt 3589,
               argDesc = "Server port" } ]

run_service :: Int -> IO ()
run_service port = do
  master <- masterInit port
  forever $ do
    client <- masterAccept master
    hClose client

run_game (port_w, port_b) = do
  handle_w <- connectionInit port_w 'W'
  handle_b <- connectionInit port_b 'B'
  doGame (Just 600000) (handle_w, handle_b)

main :: IO ()
main = do
  args <- parseArgsIO ArgsComplete argd
  let port = fromJust (getArgInt args OptionPort)
---  withSocketsDo (run_service port)
  withSocketsDo $ run_game (port, port + 1)
