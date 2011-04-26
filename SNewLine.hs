--- Copyright © 2010 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "MIT License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

--- Code to deal with emitting telnet-appropriate line
--- endings

module SNewLine (sNewLine, sUnlines, sPutStrLn)
where

import System.IO

import Log

sNewLine :: String
sNewLine = "\r\n"

sUnlines :: [String] -> String
sUnlines = concatMap (++ sNewLine)

sPutStrLn :: MonadLogIO m => Handle -> String -> m ()
sPutStrLn h s = liftIO $ do
  hPutStr h $ s ++ sNewLine
  hFlush h

