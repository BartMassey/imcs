--- Copyright © 2010 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "MIT License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

--- Code to deal with emitting telnet-appropriate line
--- endings

module SNewLine (sNewLine, sUnlines, sPutStrNL, sGetLine)
where

import Data.Char
import System.IO

sNewLine :: String
sNewLine = "\r\n"

sUnlines :: [String] -> String
sUnlines = concatMap (++ sNewLine)

sPutStrNL :: Handle -> String -> IO ()
sPutStrNL h s = do
  hPutStr h $ s ++ sNewLine
  hFlush h

sGetLine :: Handle -> IO String
sGetLine h = do
  c <- hGetChar h
  case ord(c) of
    0x0d -> do
      c2 <- hLookAhead h
      case ord(c2) of
        0x00 -> do 
          _ <- hGetChar h
          return ""
        0x0a -> do 
          _ <- hGetChar h
          return ""
        _ -> do
          return ""
    0x0a -> do
      return ""
    _ -> do
      cs <- sGetLine h
      return $ c : cs
