--- Copyright © 2009 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "MIT License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

module Readline(hReadline)
where

import System.IO

chop :: String -> String
chop = dropWhile (flip elem [' ', '\t', '\r', '\n'])

chomp :: String -> String
chomp = reverse . chop . reverse . chop

hReadline :: Handle -> IO String
hReadline h = do
  line <- hGetLine h
  return (chomp line)
