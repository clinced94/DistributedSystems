module Main where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Network.Socket
import qualified Network.Socket.ByteString as NSB
import System.Environment
import System.IO
import System.Console.ANSI
import Data.List
import Data.String
import Data.Bool
import Data.Maybe
import qualified Data.ByteString.Char8 as B

type Name = String
type IP = String
type Port = String
type ID = Int

data Client = Client IP Port Name Socket ID
	deriving (Eq, Show)

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
getClientIP (Client ip _ _ _ _) = ip

getClientPort :: Client -> Port
getClientPort (Client _ port _ _ _) = port

getClientName :: Client -> Name
getClientName (Client _ _ name _ _) = name

getClientSocket :: Client -> Socket
getClientSocket (Client _ _ _ sock _) = sock

getClientID :: Client -> ID
getClientID (Client _ _ _ _ cId) = cId

findById :: [Client] -> ID -> Client
findById (c:cs) i
	| (getClientID c) == i = c
	| otherwise = findById cs i

insertChatroom :: Forum -> MVar Chatroom -> IO ()
insertChatroom forum newRm = do
	oldForum <- takeMVar forum
	putMVar forum (oldForum ++ [newRm])

createChatroom :: Name -> IDGenerator -> IO (MVar Chatroom)
createChatroom n gen = do
	newId <- getNewId gen
	newMVar (Chatroom n [] newId)

getChatroom :: Name -> [MVar Chatroom] -> IO (Maybe (MVar Chatroom))
getChatroom name [] = return Nothing
getChatroom name (room:rooms) = do
	currentChatroom <- takeMVar room
	putMVar room currentChatroom
	if name == getRoomName currentChatroom
		then return (Just room)
		else getChatroom name rooms

getChatroomByID :: ID -> [MVar Chatroom] -> IO (Maybe (MVar Chatroom))
getChatroomByID _ [] = return Nothing
getChatroomByID cId (room:rooms) = do
	currentChatroom <- takeMVar room
	putMVar room currentChatroom
	if cId == getRoomId currentChatroom
		then return (Just room)
		else getChatroomByID cId rooms

getClientByID :: ID -> [Client] -> Maybe (Client)
getClientByID _ [] = Nothing
getClientByID cId (c:cs)
	| cId == (getClientID c) = Just c
	| otherwise = getClientByID cId cs

addClient :: Client -> MVar Chatroom -> IO ()
addClient client room = do
	(Chatroom name clients rmId) <- takeMVar room
	putMVar room (Chatroom name (clients ++ [client]) rmId)

removeClient :: Client -> MVar Chatroom -> IO ()
removeClient client room = do
	(Chatroom name clients rmId) <- takeMVar room
	putMVar room (Chatroom name (clients \\ [client]) rmId)

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
	putStrLn $ "[LOG] Connection from:\n" ++ (show clientInfo)
	forkIO (receiveMessage usableSocket killSwitch host port (show clientInfo) forum gen)
	--_ <- forkFinally (receiveMessage usableSocket count killSwitch host port) (\_ -> endThread usableSocket count)
	serverLoop sock killSwitch host port forum gen

receiveMessage :: Socket -> MVar () -> String -> String -> String -> Forum -> IDGenerator -> IO ()
receiveMessage sock killSwitch host port clientInfo forum gen = do
	setSGR [Reset]
	message <- NSB.recv sock 4096
	setSGR [SetColor Foreground Vivid Cyan]
	if not(null (B.unpack message)) then do
		putStrLn $ "[LOG] Message:\n" ++ (B.unpack message)
		else do
			putStr ""
	setSGR [Reset]
	handleMessage sock (B.unpack message) killSwitch host port clientInfo forum gen

handleMessage :: Socket -> String -> MVar () -> String -> String -> String -> Forum -> IDGenerator -> IO ()
handleMessage s msg killSwitch host port clientInfo forum gen
	| isPrefixOf "JOIN_CHATROOM" msg = do
		enterChatroom s msg host port clientInfo forum gen
		receiveMessage s killSwitch host port clientInfo forum gen
	| isPrefixOf "LEAVE_CHATROOM" msg = do
		leaveChatroom s msg forum
		receiveMessage s killSwitch host port clientInfo forum gen
	| isPrefixOf "DISCONNECT" msg = do
		disconnectClient s
	| isPrefixOf "HELO" msg	= do
		putStrLn "Dealing with message"
		NSB.send s (B.pack $ msg ++ "IP:" ++ host ++ "\nPort:" ++ port ++"\nStudentID:13320590\n")
		putStrLn "Response sent"
		receiveMessage s killSwitch host port clientInfo forum gen
	| isPrefixOf "KILL_SERVICE" msg	= do
		putStrLn "Killswitch Active"
		putMVar killSwitch ()
	| otherwise = do
		--putStrLn "Nothing is being done"
		receiveMessage s killSwitch host port clientInfo forum gen

enterChatroom :: Socket -> String -> String -> String -> String -> Forum -> IDGenerator -> IO ()
enterChatroom s msg host port clientInfo forum gen = do
	putStrLn "Getting Join Message"
	(roomName, clientName) <- getJoinMesgInfo msg
	chats <- takeMVar forum
	let clientIp = takeWhile (/= ':') clientInfo
	let clientPort = tail $ dropWhile (/= ':') clientInfo
	newCId <- getNewId gen
	let newClient = (Client clientIp clientPort clientName s newCId)
	foundRoom <- getChatroom roomName chats
	if (isJust foundRoom)
		then do
			addClient newClient (fromJust foundRoom)
			chatroom <- takeMVar (fromJust foundRoom)
			setSGR [SetColor Foreground Vivid Green]
			sendJoinResponse s newClient port host chatroom
			setSGR [SetColor Foreground Dull Red]
			broadcastJoin newClient chatroom
			setSGR [Reset]
			putMVar (fromJust foundRoom) chatroom
			putMVar forum chats
			else do
				newChatroom <- createChatroom roomName gen
				addClient newClient newChatroom
				newChat <- takeMVar newChatroom
				setSGR [SetColor Foreground Vivid Green]
				sendJoinResponse s newClient port host newChat
				setSGR [SetColor Foreground Dull Red]
				broadcastJoin newClient newChat
				setSGR [Reset]
				putMVar newChatroom newChat
				putMVar forum (chats ++ [newChatroom])

printAllChatrooms :: [MVar Chatroom] -> IO () -- DEBUG METHOD
printAllChatrooms [] = putStrLn "[]"
printAllChatrooms (room:rooms) = do
	curr <- takeMVar room
	putMVar room curr
	putStrLn $ getRoomName curr
	printAllChatrooms rooms

getJoinMesgInfo :: String -> IO (String,String)
getJoinMesgInfo msg = do
	putStrLn msg
	return (roomName, clientName) where
	mgsLines = lines msg
	roomName = drop 15 (mgsLines !! 0)
	clientName = drop 13 (mgsLines !! 3)

sendJoinResponse :: Socket -> Client -> String -> String -> Chatroom -> IO ()
sendJoinResponse s c serverPort serverIP chatroom = do
	let responseMsg = clientJoinResponse c serverIP serverPort chatroom
	putStrLn $ "Response: \n" ++ responseMsg
	NSB.send s $ B.pack responseMsg
	return ()

clientJoinResponse :: Client -> String -> String -> Chatroom -> String
clientJoinResponse c serverIP serverPort chatroom = "JOINED_CHATROOM: " ++ (getRoomName chatroom) ++ "\nSERVER_IP: " ++ serverIP ++ "\nPORT: " ++ serverPort ++ "\nROOM_REF: " ++ show (getRoomId chatroom) ++ "\nJOIN_ID: " ++ show (getClientID c) ++ "\n"

broadcastJoin :: Client -> Chatroom -> IO ()
broadcastJoin c ch = do
	let broadcastMsg = chatroomJoinBroadcast c ch
	putStrLn "Broadcasting..."
	mapM_ (\y -> putStrLn (show y)) (getRoomClients ch)
	mapM_ (\x -> sendToClient x broadcastMsg) (getRoomClients ch)
	putStrLn "Broadcast sent!"

chatroomJoinBroadcast :: Client -> Chatroom -> String
chatroomJoinBroadcast c chatroom = "CHAT:" ++ show (getRoomId chatroom) ++ "\nCLIENT_NAME: " ++ (getClientName c) ++ "\nMESSAGE:User " ++ (getClientName c) ++ " has joined.\n\n"

leaveChatroom :: Socket -> String -> Forum -> IO ()
leaveChatroom s msg forumMV = do
	putStrLn "Getting Leave Message"
	let (chatroomID, clientID, clientName) = getLeaveMessageInfo msg
	forum <- takeMVar forumMV
	maybeChatroomMV <- getChatroomByID (read chatroomID) forum
	if isJust maybeChatroomMV
		then do
			let chatroomMV = fromJust maybeChatroomMV
			chatroom <- takeMVar chatroomMV
			let maybeClient = getClientByID (read clientID) $ getRoomClients chatroom
			if isJust maybeClient
				then do
					putStrLn "Both room and client are present"
					setSGR [SetColor Foreground Vivid Green]
					sendLeaveResponse s (fromJust maybeClient) chatroom
					setSGR [SetColor Foreground Dull Red]
					broadcastLeave (fromJust maybeClient) chatroom
					setSGR [Reset]
					putMVar chatroomMV (Chatroom (getRoomName chatroom) ((getRoomClients chatroom) \\ [(fromJust maybeClient)]) (getRoomId chatroom))
					putMVar forumMV forum
					else do
						putStrLn "Room is present"
						setSGR [SetColor Foreground Vivid Green]
						sendPseudoLeaveResponse s clientID chatroomID
						setSGR [SetColor Foreground Dull Red]
						broadcastPseudoLeave clientID clientName chatroom
						setSGR [Reset]
						putMVar chatroomMV chatroom
						putMVar forumMV forum
				else do
					putStrLn "Room is not present"
					setSGR [SetColor Foreground Vivid Green]
					sendPseudoLeaveResponse s clientID chatroomID
					setSGR [Reset]
					putMVar forumMV forum

getLeaveMessageInfo :: String -> (String, String, String)
getLeaveMessageInfo msg = (chId, clId, clNm) where
	msgLines = lines msg
	chId = drop 16 (msgLines !! 0)
	clId = drop 9 (msgLines !! 1)
	clNm = drop 13 (msgLines !! 2)

broadcastLeave :: Client -> Chatroom -> IO ()
broadcastLeave cl ch = do
	let broadcastMsg = chatroomLeaveBroadcast cl ch
	putStrLn "Broadcasting leave:"
	putStrLn broadcastMsg
	mapM_ (\x -> NSB.send (getClientSocket x) $ B.pack broadcastMsg) (getRoomClients ch)
	putStrLn "Leave broadcast sent!"

broadcastPseudoLeave :: String -> String -> Chatroom -> IO ()
broadcastPseudoLeave clId clNm ch = do
	let broadcastMsg = chatroomPseudoLeaveBroadcast clId clNm ch
	putStrLn "Broadcasting leave:"
	putStrLn broadcastMsg
	mapM_ (\x -> NSB.send (getClientSocket x) $ B.pack broadcastMsg) (getRoomClients ch)
	putStrLn "Leave broadcast sent!"

chatroomLeaveBroadcast :: Client -> Chatroom -> String
chatroomLeaveBroadcast cl ch = "CHAT: " ++ show (getRoomId ch) ++ "\nCLIENT_NAME:" ++ (getClientName cl) ++ "\nMESSAGE:" ++ (getClientName cl) ++ " has left " ++ (getRoomName ch) ++ ".\n\n"

sendLeaveResponse :: Socket -> Client -> Chatroom -> IO ()
sendLeaveResponse s cl ch = do
	let responseMsg = clientLeaveResponse cl ch
	putStrLn $ "Response:\n" ++ responseMsg
	NSB.send s $ B.pack responseMsg
	return ()

clientLeaveResponse :: Client -> Chatroom -> String
clientLeaveResponse cl ch = "LEFT_CHATROOM:" ++ show (getRoomId ch) ++ "\nJOIN_ID:" ++ show (getClientID cl) ++ "\n"

sendPseudoLeaveResponse :: Socket -> String -> String -> IO ()
sendPseudoLeaveResponse s clId chId = do
	let responseMsg = clientPseudoLeaveResponse clId chId
	putStrLn $ "Response:\n" ++ responseMsg
	NSB.send s $ B.pack responseMsg
	return ()

clientPseudoLeaveResponse :: String -> String -> String
clientPseudoLeaveResponse clId chId = "LEFT_CHATROOM: " ++ chId ++ "\nJOIN_ID: " ++ clId ++ "\n"

chatroomPseudoLeaveBroadcast :: String -> String -> Chatroom -> String
chatroomPseudoLeaveBroadcast clId clNm ch = "CHAT:" ++ show (getRoomId ch) ++ "\nCLIENT_NAME:" ++ clNm ++ "\nMESSAGE:" ++ clNm ++ " has left " ++ (getRoomName ch) ++ ".\n\n"

chatToRoom :: Socket -> String -> Forum -> IO ()
chatToRoom s msg forumMV = do
	putStrLn "Getting Chat Message Info"
	let (roomId, clientId, clientName, message) = getChatMessageInfo msg
	forum <- takeMVar forumMV
	maybeChatroomMV <- getChatroomByID (read roomId) forum
	if isJust maybeChatroomMV
		then do
			let chatroomMV = fromJust maybeChatroomMV
			chatroom <- takeMVar chatroomMV
			let maybeClient = getClientByID (read clientId) $ getRoomClients chatroom
			if isJust maybeClient
				then do
					setSGR [SetColor Foreground Dull Red]
					broadcastChat roomId clientName message chatroom
					setSGR [Reset]
					putMVar chatroomMV (Chatroom (getRoomName chatroom) ((getRoomClients chatroom) \\ [(fromJust maybeClient)]) (getRoomId chatroom))
					putMVar forumMV forum
					else do
						putMVar chatroomMV chatroom
						putMVar forumMV forum
				else do
					putMVar forumMV forum

getChatMessageInfo :: String -> (String, String, String, String)
getChatMessageInfo msg = (roomId, clientId, clientName, message) where
	msgLines = lines msg
	roomId = drop 6 (msgLines !! 0)
	clientId = drop 9 (msgLines !! 1)
	clientName = drop 13 (msgLines !! 2)
	message = drop 9 (msgLines !! 3)

sendToClient :: Client -> String -> IO ()
sendToClient c msg = do
	socketName <- getPeerName $ getClientSocket c
	putStrLn $ "Sending to client " ++ show (getClientID c) ++ " (" ++ show socketName ++ ") :\n" ++ msg
	NSB.send (getClientSocket c) $ B.pack msg
	return ()

broadcastChat :: String -> String -> String -> Chatroom -> IO ()
broadcastChat roomId clientName message ch = do
	let chatMessage = chatroomChatBroadcast roomId clientName message
	putStrLn "Broadcasting Chat"
	mapM_ (\x -> sendToClient x chatMessage) (getRoomClients ch)
	putStrLn "Chat Broadcast Sent!"

chatroomChatBroadcast :: String -> String -> String -> String
chatroomChatBroadcast roomId clientName message = "CHAT: " ++ roomId ++ "\nCLIENT_NAME: " ++ clientName ++ "\nMESSAGE: " ++ message ++ "\n\n"

disconnectClient :: Socket -> IO ()
disconnectClient s = close s

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
