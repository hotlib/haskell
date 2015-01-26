module Util.Util where

import System.IO
import Prelude (head, (==), Maybe(..), Bool(..), Show, read, String, Read, seq, return)
import Control.Applicative
import System.Directory
import Data.Text hiding (map, lines)
import Import 

codesFile :: String
codesFile = "codes.txt"

adminFile :: String
adminFile = "admin.txt"

readCodes :: IO [Text]
readCodes = do
        s <- getData codesFile
        case s of
            Just x -> return x
            Nothing -> return []


readLine :: (Read a) => String -> IO a
readLine f = Prelude.head <$> readLines f

readLines :: (Read a) => String -> IO [a]
readLines f = do
  outh <- openFile f ReadMode
  s <- System.IO.hGetContents outh
  let result = map read $ lines s
  result `seq` hClose outh
  return result

getData :: (Read a, Show a) =>  String -> IO (Maybe a)
getData f = do
  exists <- doesFileExist f
  case exists of 
       False -> return Nothing
       True -> Just <$> (readLine f)

appendData :: (Show a) => a -> String -> IO ()
appendData s f =  do 
  outh <- openFile f AppendMode 
  hPrint outh s
  hClose outh 

saveData :: (Show a) => a -> String -> IO ()
saveData s f =  do 
  outh <- openFile f WriteMode
  hPrint outh s
  hClose outh 

readAdminPassword :: IO Text 
readAdminPassword = readLine adminFile

writeAdminPassword :: Text -> IO ()
writeAdminPassword p = saveData p adminFile 

isAdmin :: Text -> IO Bool
isAdmin p = (p == ) <$> readAdminPassword

authCode :: Handler Html -> Handler Html -> Handler Html
authCode userPage adminPage = do
  x <- lookupSession "logged"
  case x of 
       Just y -> (liftIO $ isAdmin y) >>= \t -> if t then adminPage else userPage 
       Nothing -> redirect LoginR
 
setSessionCode :: MonadHandler m => Text -> m ()
setSessionCode c = setSession "logged" c
