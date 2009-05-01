--- Copyright © 2009 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "MIT License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

module Connection(Socket, masterInit, masterAccept, connectionInit)
where

import Network
import Network.Socket as NS
import System.IO

masterInit :: Int -> IO Socket
masterInit port_num = do
  hPutStrLn stderr ("listening for game on port " ++ show port_num)
  listenOn (PortNumber . fromIntegral $  port_num)

masterAccept :: Socket -> IO Handle
masterAccept listen_socket = do
  (handle, hostname, client_port) <- Network.accept listen_socket
  hPutStrLn stderr ("handling host " ++ hostname ++
                    " port " ++ show client_port)
  hSetBuffering handle LineBuffering
  return handle

connectionInit :: Int -> Char -> IO Handle
connectionInit port_num side = do
  hPutStrLn stderr ("listening for " ++ [side] ++
                    " on port " ++ show port_num)
  listen_socket <- listenOn (PortNumber . fromIntegral $ port_num)
  (handle, hostname, _) <- Network.accept listen_socket
  hPutStrLn stderr ("got " ++ [side] ++
                    " host " ++ hostname ++
                    " on port " ++ show port_num)
  hPutStrLn handle [side]
  hSetBuffering handle LineBuffering
  return handle
