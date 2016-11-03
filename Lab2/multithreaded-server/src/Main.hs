module Main where

import Network.Socket

main :: IO ()
main = do 
	sock <- socket AF_INET Stream 0
	setSocketOption sock ReuseAddr 1
	bind sock (SockAddrInet 7000 iNADDR_ANY)
	listen sock 2
	mainLoop sock

mainLoop :: Socket -> IO()
mainLoop sock = do
	conn <- accept sock
	runConn conn
	mainLoop sock

runConn :: (Socket, SockAddr) -> IO()
runConn (sock, _) = do
	send sock "Hello\n"
	close sock
