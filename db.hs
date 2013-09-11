{-# LANGUAGE OverloadedStrings #-}

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import Data.ByteString.Char8 (pack)
import Control.Applicative

type TenantId = Int

commonConnection :: ConnectInfo
commonConnection = defaultConnectInfo {connectHost = "XXX", connectPort = 5432, connectUser = "XXX", connectPassword = "XXX", connectDatabase = "XXX"}

tenantConnection :: ConnectInfo
tenantConnection = defaultConnectInfo {connectHost = "XXX", connectPort = 5432, connectUser = "XXX", connectPassword = "XXX", connectDatabase = "XXX"}


tenantIds :: IO [TenantId]
tenantIds = do
   c <- connect commonConnection 
   result <- query_ c "select XXX from XXX" :: IO [Only TenantId] -- id from tenant
   let ids = foldl (\a c -> fromOnly c : a) [] result
   return ids 

tenantData :: [TenantId] -> IO [String]
tenantData ids = do
   
   c <- connect tenantConnection 
   resultSet <- sequence $ queries c selects
   return (extractValues resultSet)  
   where 
   		selects = map (\x -> "select XXX from t" ++  (show x) ++ ".XXX") ids
   		extractValues rs = foldl (\a c -> fromOnly c : a) [] (concat rs) 
   		queries c s = map (\x -> (query_ c (Query $ pack x)) :: IO [Only String] ) s

main :: IO ()
main = do
   ids <- tenantIds
   uploadData <- tenantData ids
   print ids
   print uploadData
   return ()

