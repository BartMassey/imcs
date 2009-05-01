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
import Control.Concurrent
import Control.Concurrent.MVar

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

connection_helper :: Int -> Char -> IO (MVar Handle)
connection_helper port side =  do
  m <- newEmptyMVar
  forkIO $ do
    handle <- connectionInit port side
    putMVar m handle
  return m

run_game :: (Int, Int) -> IO ()
run_game (port_w, port_b) = do
  m_w <- connection_helper port_w 'W'
  m_b <- connection_helper port_b 'B'
  h_w <- takeMVar m_w
  h_b <- takeMVar m_b
  doGame (Just 600000) (h_w, h_b)

run_service :: Int -> IO ()
run_service port = do
  master <- masterInit port
  forever $ do
    client <- masterAccept master
    hClose client

main :: IO ()
main = do
  args <- parseArgsIO ArgsComplete argd
  let port = fromJust (getArgInt args OptionPort)
---  withSocketsDo (run_service port)
  withSocketsDo $ run_game (port, port + 1)
