
import Network
import System.IO
import Control.Concurrent
import Data.Time
import Control.Applicative
import Data.Maybe
import Control.Exception
import System.Exit
import Data.Sequence hiding (replicate)
import Control.Monad
import Data.ByteString.Char8 hiding (replicate, putStrLn, hPutStr, hGetLine)
import Network.IRC

host :: HostName
host = "127.0.0.1"

port :: PortID 
port = PortNumber 6667 

hExistsInput :: Handle -> IO Bool
hExistsInput h = catch (hWaitForInput h 500) handler 
	where 
		handler :: IOException -> IO Bool
		handler e = do
			let msg = "A connection problem occured:  " ++ show e
			pprint $ Just msg 
			exitFailure

hGetReadyLine :: Handle -> IO (Maybe String)
hGetReadyLine h = hExistsInput h >>= hRead 
	where
		hRead True = Just <$> hGetLine h
		hRead False = return Nothing

pprint :: Maybe String -> IO ()
pprint line
	| isJust line && isJust msg = getCurrentTime >>= \t -> (putStrLn $ (show t) ++ "   " ++ (show . fromJust $ msg))
	| otherwise   = return ()
	where 
		msg = (decode . pack . fromJust $ line)

getInput :: Handle -> IO ()
getInput h = hGetReadyLine h >>= pprint
	
write :: Handle -> String -> IO ()
write h msg = do
	let debugMsg = "Sending to server: " ++ msg
	pprint $ Just debugMsg 
	hPutStr h (msg ++ "\r\n")

readServer :: Handle -> Int -> IO ()
readServer h n = do 
	let lines = replicate n (getInput h)
	sequence_ lines 

ircLogin :: Handle -> IO ()
ircLogin h = do
	write h "NICK hbot"
	write h "USER hbot localhost localhost :Hbot"
	write h "JOIN #mychannel"

main = do
	h <- connectTo host port 
	hSetBuffering h NoBuffering 
	-- --------------
	let rs2 = readServer h 2
	let rs30 = readServer h 30
	let login = ircLogin h
	rs2
	login
	threadDelay 5000000
	forever rs30

	-- threadDelay 5000000
	
	-- --------------
	hClose h
	print "the end"