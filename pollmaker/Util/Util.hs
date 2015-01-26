module Util.Util where

import System.IO
import Prelude ((==), Maybe(..), Bool(..), Show, read, String, Read, seq, return)
import Control.Applicative
import System.Directory
import Data.Text

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

readData :: (Read a) => String -> IO a
readData f = do 
  outh <- openFile f ReadMode
  s <- (read <$> System.IO.hGetContents outh )
  s `seq` hClose outh
  return s

getData :: (Read a, Show a) =>  String -> IO (Maybe a)
getData f = do
  exists <- doesFileExist f
  case exists of 
       False -> return Nothing
       True -> Just <$> (readData f)


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

readAdminPasswd :: IO Text 
readAdminPasswd = readData adminFile

writeAdminPasswd :: Text -> IO ()
writeAdminPasswd p = saveData p adminFile 

isAdmin :: Text -> IO Bool
isAdmin p = (p == ) <$> readAdminPasswd



