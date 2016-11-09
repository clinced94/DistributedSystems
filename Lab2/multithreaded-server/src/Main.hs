module Main where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Network.Socket
import Network.Socket.ByteString
import System.Environment
import System.IO
import Data.List
import Data.String
import Data.Bool
import Data.ByteString.Char8

incSocketCount :: MVar Int -> IO ()
incSocketCount count = do
	num <- takeMVar count
	putMVar count (num + 1)

decSocketCount :: MVar Int -> IO ()
decSocketCount count = do
	num <- takeMVar count
	putMVar count (num - 1)

initSocket :: IO Socket
initSocket = do
	newSocket <- socket AF_INET Stream 0
	setSocketOption newSocket ReuseAddr 1
	bind newSocket $ SockAddrInet 7000 iNADDR_ANY
	return newSocket

receiveMessage :: Socket -> MVar Int -> IO ()
receiveMessage sock count = do
	message <- Network.Socket.recv sock 2048
	System.IO.putStrLn $ "Message: " ++ message
	handleMessage sock message count

handleMessage :: Socket -> String -> MVar Int -> IO ()
handleMessage s msg count 	| startswith msg "HELO text" 	= Network.Socket.ByteString.send s (pack msg) >> return ()
							| startswith msg "KILL_SERVICE"	= endThread s count
							| otherwise = return ()

endThread :: Socket -> MVar Int -> IO ()
endThread s count = do
	close s
	incSocketCount count

server :: Socket -> IO ()
server sock = do 
	listen sock 4
	count <- newMVar 4
	serverLoop sock count

serverLoop :: Socket -> MVar Int -> IO ()
serverLoop sock count = do
	(usableSocket,clientInfo) <- accept sock
	System.IO.putStrLn $ "Connection from: " ++ (show clientInfo)
	currentCount <- takeMVar count
	putMVar count currentCount
	if currentCount == 0
		then do
			close usableSocket
			serverLoop sock count
			else do
				decSocketCount count
				_ <- forkFinally (receiveMessage usableSocket count) (\_ -> endThread usableSocket count)
				serverLoop sock count

main :: IO ()
main = do
	newSocket <- initSocket
	System.IO.putStrLn "Starting server..."
	_ <- forkIO $ server newSocket
	System.IO.putStrLn "Terminating server"

startswith :: String -> String -> Bool
startswith [] _ = True
startswith (x:xs) (y:ys) 	| x == y = startswith xs ys
							| otherwise = False
