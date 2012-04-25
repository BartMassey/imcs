--- Copyright © 2010 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "MIT License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

--- Code to deal with emitting telnet-appropriate line
--- endings

module SNewLine (sNewLine, sUnlines, sPutStrNL)
where

import System.IO

sNewLine :: String
sNewLine = "\r\n"

sUnlines :: [String] -> String
sUnlines = concatMap (++ sNewLine)

sPutStrNL :: Handle -> String -> IO ()
sPutStrNL h s = do
  hPutStr h $ s ++ sNewLine
  hFlush h
