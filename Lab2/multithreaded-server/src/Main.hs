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
	addr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "7000")
	bind newSocket (addrAddress addr)
	return newSocket

receiveMessage :: Socket -> MVar Int -> MVar () -> IO ()
receiveMessage sock count killSwitch = do
	message <- Network.Socket.recv sock 4096
	System.IO.putStrLn $ "Message: " ++ message
	handleMessage sock message count killSwitch

handleMessage :: Socket -> String -> MVar Int -> MVar () -> IO ()
handleMessage s msg count killSwitch 	| startswith "HELO text" msg	= System.IO.putStrLn "Dealing with message" >> Network.Socket.ByteString.send s (pack $ msg ++ "IP:127.0.0.1\nPort:[port number]\nStudentID:13320590\n") >> return ()
										| startswith "KILL_SERVICE" msg	= System.IO.putStrLn "Killswitch Active" >> putMVar killSwitch ()
										| otherwise = System.IO.putStrLn "Nothing is being done" >> return ()

endThread :: Socket -> MVar Int -> IO ()
endThread s count = do
	close s
	incSocketCount count

server :: Socket -> MVar () -> IO ()
server sock killSwitch = do 
	listen sock 4
	count <- newMVar 4
	serverLoop sock count killSwitch

serverLoop :: Socket -> MVar Int -> MVar () -> IO ()
serverLoop sock count killSwitch = do
	(usableSocket,clientInfo) <- accept sock
	System.IO.putStrLn $ "Connection from: " ++ (show clientInfo)
	currentCount <- takeMVar count
	putMVar count currentCount
	if currentCount == 0
		then do
			close usableSocket
			serverLoop sock count killSwitch
			else do
				decSocketCount count
				_ <- forkFinally (receiveMessage usableSocket count killSwitch) (\_ -> endThread usableSocket count)
				serverLoop sock count killSwitch

main :: IO ()
main = do
	newSocket <- initSocket
	System.IO.putStrLn "Starting server..."
	killSwitch <- newEmptyMVar
	_ <- forkIO $ server newSocket killSwitch
	takeMVar killSwitch
	System.IO.putStrLn "Terminating server"

startswith :: String -> String -> Bool
startswith [] _ = True
startswith (x:xs) (y:ys) 	| x == y = startswith xs ys
							| otherwise = False
