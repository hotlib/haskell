{-# LANGUAGE OverloadedStrings #-}

import Database.PostgreSQL.Simple

myConnection :: ConnectInfo
myConnection = defaultConnectInfo {connectHost = "127.0.0.1", connectPort = 5432, connectUser = "user", connectPassword = "password", connectDatabase = "db"}

emails :: IO [Only String]
emails = do 
	conn <- connect myConnection
  	query_ conn "select datname from pg_stat_database" :: IO [Only String]
  	
main = do 
	e <- emails
	print $ extractFromOnly [] e
	where extractFromOnly a r = foldl (\a c -> fromOnly c : a) a r
