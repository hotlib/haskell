{-# LANGUAGE OverloadedStrings #-}

import Network.Wreq
import Control.Lens hiding (noneOf)
import qualified Data.ByteString.Lazy as B
import Control.Applicative ((<$>))
import Data.List.Split
import Text.Parsec

-- let mybetween = (between (char 'X') (char 'X') (many $ noneOf "X")) >>= \x ->  (char ',') >> return x)
-- ( many $ noneOf ",") >>= \x ->  (char ',') >> return x
-- p (count 3 (choice [mybetween, magic ])) "aa1aa,XaaaaX,11111,22222,33333"
--

myEndpointStatements :: String
myEndpointStatements = myEndpointRepo ++ "/statements"

myEndpointRepo :: String
myEndpointRepo = "http://localhost:8080/openrdf-sesame/repositories/palko123"

insertOptions :: Options
insertOptions = defaults & headers %~ h' 
   where 
    h' = (++) [("Content-Type", "application/x-turtle;charset=UTF-8")]

listOptions :: Options
listOptions = defaults & headers %~ h' 
   where	
    h' = (++) [("Accept", "application/rdf+xml, */*;q=0.5"), ("Content-Type", "application/sparql-query")] 

ddatta :: B.ByteString
ddatta = "<http://example.org/person/Mark_Twain33> <http://example.org/relation/author> <http://example.org/books/Huckleberry_Finn> ."

listAllQuery :: B.ByteString
listAllQuery = "construct {?s ?p ?o} where {?s ?p ?o}"

testPost :: IO (Response B.ByteString)
testPost = postWith insertOptions myEndpointStatements ddatta

testList :: IO (Response B.ByteString)
testList = postWith listOptions myEndpointRepo listAllQuery

readData :: IO [String]
readData = lines <$> readFile "data.csv"


-- let mybetween = (between (char 'X') (char 'X') (many $ noneOf "X")) >>= \x ->  (char ',') >> return x)
-- ( many $ noneOf ",") >>= \x ->  (char ',') >> return x
-- p (count 3 (choice [mybetween, magic ])) "aa1aa,XaaaaX,11111,22222,33333"
--


removeComma :: String -> ParsecT String () Identity String
removeComma x = char ',' >> return x

parseNormalEntry :: ParsecT String () Identity String
parseNormalEntry =  many (noneOf ",") >>= removeComma


parseQuotaEntry :: ParsecT String () Identity String
parseQuotaEntry = between (char '"') (char '"') (many $ noneOf "\"") >>= removeComma

parseLine :: Int -> String -> [String]
parseLine es s = let Right x = parse (count es (choice [parseQuotaEntry, parseNormalEntry])) "" s
                 in x 


main :: IO ()
main = do 
     csv <- readData	
     let headers = splitOn "," . head $ csv
     let x = map (parseLine (length headers - 1)) $ tail csv 
     print $ x !! 111
     -- l <- flip (!!) 1 <$> readData
     -- print $ parseLine 10 l
     


