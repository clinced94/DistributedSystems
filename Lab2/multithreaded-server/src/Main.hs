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

startswith :: String -> String -> Bool
startswith [] _ = True
startswith (x:xs) (y:ys) 	| x == y = startswith xs ys
							| otherwise = False

incSocketCount :: MVar Int -> IO ()
incSocketCount count = do
	num <- takeMVar count
	putMVar count (num + 1)

decSocketCount :: MVar Int -> IO ()
decSocketCount count = do
	num <- takeMVar count
	putMVar count (num - 1)

initSocket :: String -> String -> IO Socket
initSocket host port = do
	newSocket <- socket AF_INET Stream 0
	setSocketOption newSocket ReuseAddr 1
	addr:_ <- getAddrInfo Nothing (Just host) (Just port)
	bind newSocket (addrAddress addr)
	return newSocket

receiveMessage :: Socket -> MVar Int -> MVar () -> String -> String -> IO ()
receiveMessage sock count killSwitch host port = do
	message <- Network.Socket.ByteString.recv sock 4096
	System.IO.putStrLn $ "Message: " ++ (unpack message)
	handleMessage sock (unpack message) count killSwitch host port

handleMessage :: Socket -> String -> MVar Int -> MVar () -> String -> String -> IO ()
handleMessage s msg count killSwitch host port 	| startswith "HELO" msg	= do 
													System.IO.putStrLn "Dealing with message" 
													Network.Socket.ByteString.send s (pack $ msg ++ "IP:" ++ host ++ "\nPort:" ++ port ++"\nStudentID:13320590\n")
													System.IO.putStrLn "Response sent" 
													return ()
												| startswith "KILL_SERVICE" msg	= do
													System.IO.putStrLn "Killswitch Active"
													putMVar killSwitch ()
												| otherwise = do 
													System.IO.putStrLn "Nothing is being done"
													return ()

endThread :: Socket -> MVar Int -> IO ()
endThread s count = do
	close s
	incSocketCount count

server :: Socket -> MVar () -> String -> String -> IO ()
server sock killSwitch host port = do 
	listen sock 4
	count <- newMVar 4
	serverLoop sock count killSwitch host port

serverLoop :: Socket -> MVar Int -> MVar () -> String -> String -> IO ()
serverLoop sock count killSwitch host port = do
	(usableSocket,clientInfo) <- accept sock
	System.IO.putStrLn $ "Connection from: " ++ (show clientInfo)
	currentCount <- takeMVar count
	putMVar count currentCount
	if currentCount == 0
		then do
			close usableSocket
			serverLoop sock count killSwitch host port
			else do
				decSocketCount count
				_ <- forkFinally (receiveMessage usableSocket count killSwitch host port) (\_ -> endThread usableSocket count)
				serverLoop sock count killSwitch host port

main :: IO ()
main = do
	[port, host] <- getArgs
	System.IO.putStrLn $ "Starting server on " ++ host ++ ":" ++ port
	newSocket <- initSocket host port
	killSwitch <- newEmptyMVar
	System.IO.putStrLn "Server ready"
	_ <- forkIO $ server newSocket killSwitch host port
	takeMVar killSwitch
	System.IO.putStrLn "Terminating server"
