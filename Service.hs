--- Copyright © 2009 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "MIT License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

module Service(GameState(..), doCommands) where

import Control.Monad
import System.IO
import Control.Concurrent
import Control.Concurrent.MVar

import Log
import Game

type PlayerState = Maybe (String, CState)
data GameState = GameState PlayerState PlayerState

doCommands :: Handle -> MVar [GameState] -> LogIO ()
doCommands client state = do
    doGame (stdout, Just 600000) (stdout, Just 600000)
