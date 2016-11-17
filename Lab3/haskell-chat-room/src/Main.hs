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
import Data.Maybe
import qualified Data.ByteString.Char8 as B

type Name = String
type IP = String
type Port = String

data Client = Client IP Port Name
data Chatroom = Chatroom Name [Client]

startswith :: String -> String -> Bool
startswith [] _ = True
startswith (x:xs) (y:ys)
	| x == y = startswith xs ys
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

receiveMessage :: Socket -> MVar () -> String -> String -> IO ()
receiveMessage sock killSwitch host port = do
	message <- Network.Socket.ByteString.recv sock 4096
	System.IO.putStrLn $ "Message: " ++ (B.unpack message)
	handleMessage sock (B.unpack message) killSwitch host port

handleMessage :: Socket -> String -> MVar () -> String -> String -> IO ()
handleMessage s msg killSwitch host port
	| startswith "JOIN_CHATROOM" msg = enterChatroom msg host port
	| startswith "HELO" msg	= do
		System.IO.putStrLn "Dealing with message"
		Network.Socket.ByteString.send s (B.pack $ msg ++ "IP:" ++ host ++ "\nPort:" ++ port ++"\nStudentID:13320590\n")
		System.IO.putStrLn "Response sent"
		return ()
	| startswith "KILL_SERVICE" msg	= do
		System.IO.putStrLn "Killswitch Active"
		putMVar killSwitch ()
	| otherwise = do
		System.IO.putStrLn "Nothing is being done"
		return ()

enterChatroom :: String -> String -> String -> IO ()
enterChatroom msg port host = do
	(room, client) <- getMesgInfo msg
	putStrLn room

getMesgInfo :: String -> IO (String,String)
getMesgInfo msg = return (roomName, clientName) where
	mgsLines = lines msg
	roomName = drop 15 (mgsLines !! 0)
	clientName = drop 13 (mgsLines !! 3)

endThread :: Socket -> MVar Int -> IO ()
endThread s count = do
	close s
	incSocketCount count

server :: Socket -> MVar () -> String -> String -> IO ()
server sock killSwitch host port = do
	let rooms = []
	listen sock 4
	serverLoop sock killSwitch host port rooms

serverLoop :: Socket -> MVar () -> String -> String -> [Chatroom] -> IO ()
serverLoop sock killSwitch host port rooms = do
	(usableSocket,clientInfo) <- accept sock
	System.IO.putStrLn $ "Connection from: " ++ (show clientInfo)
	forkIO (receiveMessage usableSocket killSwitch host port)
	--_ <- forkFinally (receiveMessage usableSocket count killSwitch host port) (\_ -> endThread usableSocket count)
	serverLoop sock killSwitch host port rooms

findChatroom :: String -> [Chatroom] -> Maybe Chatroom
findChatroom _ [] = Nothing
findChatroom name (room:rooms)
	| roomName room == name = Just room
	| otherwise = findChatroom name rooms

roomName :: Chatroom -> String
roomName (Chatroom name _) = name

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
