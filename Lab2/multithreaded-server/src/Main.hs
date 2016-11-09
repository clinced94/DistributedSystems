module Main where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Network.Socket
import System.Environment
import System.IO
import Data.List

main :: IO ()
main = withSocketsDo $ do
  newSocket <- socket AF_INET Stream 0 
  setSocketOption newSocket ReuseAddr 1
  bind newSocket $ SockAddrInet 7000 iNADDR_ANY
  listen newSocket 4
  runServer echo newSocket 4

runServer :: (String -> String) -> Socket -> Int -> IO()
runServer f s count = forever $ do
	(usableSocket,clientAddr) <- accept s
	forkIO $ interactWithSocket f usableSocket clientAddr (count - 1)

interactWithSocket :: (String -> String) -> Socket -> SockAddr -> Int -> IO()
interactWithSocket _ _ _ 0 = do
	print "Too many clients. Go away."
interactWithSocket f s info _ = do
	let (addrIP, addrPort) = getSocketInfo (show info) []
	print ("HELO text\nIP:" ++ addrIP ++ "\nPort:" ++ addrPort ++ "\nStudentID:13320590\n")
	handle <- socketToHandle s ReadWriteMode
	forever $ f <$> System.IO.hGetLine handle >>= System.IO.hPutStrLn handle

echo :: String -> String  
echo = id

getSocketInfo :: [Char] -> [Char] -> ([Char], [Char])
getSocketInfo (x:xs) ys | x == ':' = (reverse ys, xs)
						| otherwise = getSocketInfo xs (x:ys)
