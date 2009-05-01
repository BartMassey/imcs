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
