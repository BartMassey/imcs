--- Copyright © 2009 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "MIT License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

module Service(doCommands) where

import Data.Maybe
import Control.Monad
import System.IO
import Control.Concurrent
import Control.Concurrent.MVar

doCommands :: IO ()
doCommands = return ()
