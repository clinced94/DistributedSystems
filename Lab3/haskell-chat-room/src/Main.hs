module Main where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Network.Socket
import qualified Network.Socket.ByteString as NSB
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

type Forum = MVar [MVar Chatroom]

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

----------------------------------------
-- Forum, Chatroom & Client Functions --
----------------------------------------
initForum :: IO (Forum)
initForum = do
	chats <- initChatrooms
	forum <- newMVar chats
	return forum

initChatrooms :: IO ([MVar Chatroom])
initChatrooms = return []

getRoomName :: Chatroom -> Name
getRoomName (Chatroom name _ _) = name

getRoomClients :: Chatroom -> [Client]
getRoomClients (Chatroom _ clients _) = clients

getRoomId :: Chatroom -> ID
getRoomId (Chatroom _ _ rmId) = rmId

isRoomEmpty :: Chatroom -> Bool
isRoomEmpty (Chatroom _ clients _) = null clients

getClientIP :: Client -> IP
getClientIP (Client ip _ _ _) = ip

getClientPort :: Client -> Port
getClientPort (Client _ port _ _) = port

getClientName :: Client -> Name
getClientName (Client _ _ name _) = name

getClientID :: Client -> ID
getClientID (Client _ _ _ cId) = cId

insertChatroom :: Forum -> MVar Chatroom -> IO ()
insertChatroom forum newRm = do
	oldForum <- takeMVar forum
	putMVar forum (oldForum ++ [newRm])

createChatroom :: Name -> IDGenerator -> IO (MVar Chatroom)
createChatroom n gen = do
	newId <- getNewId gen
	newMVar (Chatroom n [] newId)

getChatroom :: Name -> [MVar Chatroom] -> IO (Maybe (MVar Chatroom))
getChatroom name [] = do
	putStrLn "DEBUG: getChatroom is returning Nothing\n"
	return Nothing
getChatroom name (room:rooms) = do
	currentChatroom <- takeMVar room
	putMVar room currentChatroom
	if name == getRoomName currentChatroom
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

server :: Socket -> MVar () -> String -> String -> Forum -> IDGenerator -> IO ()
server sock killSwitch host port forum gen = do
	listen sock 4
	serverLoop sock killSwitch host port forum gen

serverLoop :: Socket -> MVar () -> String -> String -> Forum -> IDGenerator -> IO ()
serverLoop sock killSwitch host port forum gen = do
	(usableSocket,clientInfo) <- accept sock
	System.IO.putStrLn $ "Connection from: " ++ (show clientInfo)
	forkIO (receiveMessage usableSocket killSwitch host port (show clientInfo) forum gen)
	--_ <- forkFinally (receiveMessage usableSocket count killSwitch host port) (\_ -> endThread usableSocket count)
	serverLoop sock killSwitch host port forum gen

receiveMessage :: Socket -> MVar () -> String -> String -> String -> Forum -> IDGenerator -> IO ()
receiveMessage sock killSwitch host port clientInfo forum gen = do
	message <- NSB.recv sock 4096
	System.IO.putStrLn $ "Message: " ++ (B.unpack message)
	handleMessage sock (B.unpack message) killSwitch host port clientInfo forum gen

handleMessage :: Socket -> String -> MVar () -> String -> String -> String -> Forum -> IDGenerator -> IO ()
handleMessage s msg killSwitch host port clientInfo forum gen
	| startswith "JOIN_CHATROOM" msg = enterChatroom s msg host port clientInfo forum gen
	| startswith "HELO" msg	= do
		System.IO.putStrLn "Dealing with message"
		NSB.send s (B.pack $ msg ++ "IP:" ++ host ++ "\nPort:" ++ port ++"\nStudentID:13320590\n")
		System.IO.putStrLn "Response sent"
		return ()
	| startswith "KILL_SERVICE" msg	= do
		System.IO.putStrLn "Killswitch Active"
		putMVar killSwitch ()
	| otherwise = do
		System.IO.putStrLn "Nothing is being done"
		return ()

enterChatroom :: Socket -> String -> String -> String -> String -> Forum -> IDGenerator -> IO ()
enterChatroom s msg port host clientInfo forum gen = do
	(roomName, clientName) <- getMesgInfo msg
	putStrLn roomName
	chats <- takeMVar forum
	foundRoom <- getChatroom roomName chats
	if (isJust foundRoom)
		then do
			let clientIp = takeWhile (/= ':') clientInfo
			let clientPort = tail $ dropWhile (/= ':') clientInfo
			newCId <- getNewId gen
			let newClient = (Client clientIp clientPort clientName newCId)
			addClient newClient (fromJust foundRoom)
			chatroom <- takeMVar (fromJust foundRoom)
			sendResponse s newClient host port chatroom
			putMVar (fromJust foundRoom) chatroom
			putMVar forum chats
			else do
				newChatroom <- createChatroom roomName gen
				printAllChatrooms (chats ++ [newChatroom])
				let clientIp = takeWhile (/= ':') clientInfo
				let clientPort = tail $ dropWhile (/= ':') clientInfo
				newCId <- getNewId gen
				let newClient = (Client clientIp clientPort clientName newCId)
				addClient newClient newChatroom
				newChat <- takeMVar newChatroom
				sendResponse s newClient host port newChat
				putMVar newChatroom newChat
				putMVar forum (chats ++ [newChatroom])

printAllChatrooms :: [MVar Chatroom] -> IO () -- DEBUG METHOD
printAllChatrooms [] = putStrLn "[]"
printAllChatrooms (room:rooms) = do
	curr <- takeMVar room
	putMVar room curr
	putStrLn $ getRoomName curr
	printAllChatrooms rooms

getMesgInfo :: String -> IO (String,String)
getMesgInfo msg = return (roomName, clientName) where
	mgsLines = lines msg
	roomName = drop 15 (mgsLines !! 0)
	clientName = drop 13 (mgsLines !! 3)

sendResponse :: Socket -> Client -> String -> String -> Chatroom -> IO ()
sendResponse s c serverIP serverPort chatroom = do
	let responseMsg = clientResponse c serverIP serverPort chatroom
	putStrLn $ "Response: \n" ++ responseMsg
	NSB.send s $ B.pack responseMsg
	return ()

clientResponse :: Client -> String -> String -> Chatroom -> String
clientResponse c serverIP serverPort chatroom = "JOINED_CHATROOM: " ++ (getRoomName chatroom) ++ "\nSERVER_IP: " ++ serverIP ++ "\nPORT: " ++ serverPort ++ "\nROOM_REF: " ++ show (getRoomId chatroom) ++ "\nJOIN_ID: " ++ show (getClientID c) ++ "\n\n"

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
	forum <- initForum
	gen <- newIdGenerator
	System.IO.putStrLn "Server ready"
	forkIO $ server newSocket killSwitch host port forum gen
	takeMVar killSwitch
	System.IO.putStrLn "Terminating server"
