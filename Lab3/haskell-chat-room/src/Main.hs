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
type ID = Int

data Client = Client IP Port Name ID
	deriving Eq

data Chatroom = Chatroom Name [Client] ID
	deriving Eq

------------------------------------------
-- ID Generator for Client and Chatroom --
------------------------------------------

type IDGenerator = MVar ID

newIdGenerator :: IO (MVar ID)
newIdGenerator = newMVar 0

getNewId :: IDGenerator -> IO ID
getNewId gen = do
	newId <- takeMVar gen
	putMVar gen (newId + 1)
	return newId

---------------------------------
-- Chatroom & Client Functions --
---------------------------------
initChatrooms :: IO ([MVar Chatroom])
initChatrooms = return []

roomName :: Chatroom -> Name
roomName (Chatroom name _ _) = name

roomClients :: Chatroom -> [Client]
roomClients (Chatroom _ clients _) = clients

roomId :: Chatroom -> ID
roomId (Chatroom _ _ rmId) = rmId

isEmpty :: Chatroom -> Bool
isEmpty (Chatroom _ clients _) = null clients

getClientIP :: Client -> IP
getClientIP (Client ip _ _ _) = ip

getClientPort :: Client -> Port
getClientPort (Client _ port _ _) = port

getClientName :: Client -> Name
getClientName (Client _ _ name _) = name

getClientID :: Client -> ID
getClientID (Client _ _ _ cId) = cId

addChatroom :: [MVar Chatroom] -> Name -> IDGenerator -> IO [MVar Chatroom]
addChatroom rooms n gen = do
	newCR <- createChatroom n gen
	return (rooms ++ [newCR])

createChatroom :: Name -> IDGenerator -> IO (MVar Chatroom)
createChatroom n gen = do
	newId <- getNewId gen
	newMVar (Chatroom n [] newId)

getChatroom :: Name -> [MVar Chatroom] -> IO (Maybe (MVar Chatroom))
getChatroom name [] = return Nothing
getChatroom name (room:rooms) = do
	currentChatroom <- takeMVar room
	putMVar room currentChatroom
	if name == roomName currentChatroom
		then return (Just room)
		else getChatroom name rooms

addClient :: Client -> MVar Chatroom -> IO ()
addClient client room = do
	(Chatroom name clients rmId) <- takeMVar room
	putMVar room (Chatroom name (clients ++ [client]) rmId)

removeClient :: Client -> MVar Chatroom -> IO ()
removeClient client room = do
	(Chatroom name clients rmId) <- takeMVar room
	putMVar room (Chatroom name (clients \\ [client]) rmId)

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

------------
-- Server --
------------

initSocket :: String -> String -> IO Socket
initSocket host port = do
	newSocket <- socket AF_INET Stream 0
	setSocketOption newSocket ReuseAddr 1
	addr:_ <- getAddrInfo Nothing (Just host) (Just port)
	bind newSocket (addrAddress addr)
	return newSocket

server :: Socket -> MVar () -> String -> String -> [MVar Chatroom] -> IDGenerator -> IO ()
server sock killSwitch host port chats gen = do
	listen sock 4
	serverLoop sock killSwitch host port chats gen

serverLoop :: Socket -> MVar () -> String -> String -> [MVar Chatroom] -> IDGenerator -> IO ()
serverLoop sock killSwitch host port chats gen = do
	(usableSocket,clientInfo) <- accept sock
	System.IO.putStrLn $ "Connection from: " ++ (show clientInfo)
	forkIO (receiveMessage usableSocket killSwitch host port (show clientInfo) chats gen)
	--_ <- forkFinally (receiveMessage usableSocket count killSwitch host port) (\_ -> endThread usableSocket count)
	serverLoop sock killSwitch host port chats gen

receiveMessage :: Socket -> MVar () -> String -> String -> String -> [MVar Chatroom] -> IDGenerator -> IO ()
receiveMessage sock killSwitch host port clientInfo chats gen = do
	message <- Network.Socket.ByteString.recv sock 4096
	System.IO.putStrLn $ "Message: " ++ (B.unpack message)
	handleMessage sock (B.unpack message) killSwitch host port clientInfo chats gen

handleMessage :: Socket -> String -> MVar () -> String -> String -> String -> [MVar Chatroom] -> IDGenerator -> IO ()
handleMessage s msg killSwitch host port clientInfo chats gen
	| startswith "JOIN_CHATROOM" msg = enterChatroom msg host port clientInfo chats gen
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

enterChatroom :: String -> String -> String -> String -> [MVar Chatroom] -> IDGenerator -> IO ()
enterChatroom msg port host clientInfo chats gen = do
	(roomName, clientName) <- getMesgInfo msg
	putStrLn roomName
	foundRoom <- getChatroom roomName chats
	if (isJust foundRoom)
		then do
			let clientIp = takeWhile (/= ':') clientInfo
			let clientPort = tail $ dropWhile (/= ':') clientInfo
			newCId <- getNewId gen
			addClient (Client clientIp clientPort clientName newCId) (fromJust foundRoom)
			else do
				addChatroom chats roomName gen
				newRoom <- getChatroom roomName chats
				let clientIp = takeWhile (/= ':') clientInfo
				let clientPort = tail $ dropWhile (/= ':') clientInfo
				newCId <- getNewId gen
				addClient (Client clientIp clientPort clientName newCId) (fromJust newRoom)

getMesgInfo :: String -> IO (String,String)
getMesgInfo msg = return (roomName, clientName) where
	mgsLines = lines msg
	roomName = drop 15 (mgsLines !! 0)
	clientName = drop 13 (mgsLines !! 3)

--clientResponse :: Client -> String -> String -> IO ()
--clientResponse

endThread :: Socket -> MVar Int -> IO ()
endThread s count = do
	close s
	incSocketCount count

----------
-- Main --
----------

main :: IO ()
main = do
	[port, host] <- getArgs
	System.IO.putStrLn $ "Starting server on " ++ host ++ ":" ++ port
	newSocket <- initSocket host port
	killSwitch <- newEmptyMVar
	chats <- initChatrooms
	gen <- newIdGenerator
	System.IO.putStrLn "Server ready"
	forkIO $ server newSocket killSwitch host port chats gen
	takeMVar killSwitch
	System.IO.putStrLn "Terminating server"
