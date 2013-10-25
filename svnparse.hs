import Text.Parsec.String
import Text.Parsec.Prim
import Text.Parsec
import Text.ParserCombinators.Parsec hiding (try)
import Control.Applicative hiding (many, (<|>))
import Control.Monad
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.MultiSet as MultiSet
import Data.List (sortBy, isInfixOf)

data SvnEntry = SvnEntry { getMessage :: String, getId::Int, getName :: String, getDate :: SvnDate, getSvnActions::[SvnAction] } deriving( Show ) 
data SvnAction = SvnAction { getAction :: Char, getPath :: String} deriving( Show ) 
data SvnDate = SvnDate {getYear :: Integer , getMonth :: Integer, getDay :: Integer,  getHour :: Integer, getMinute :: Integer, getDayName :: String   } deriving( Show ) 


svnChanges :: Parser SvnAction
svnChanges =  liftA2 SvnAction (spaces >> anyChar) (spaces >> (many $ noneOf "\n\r"))
				
svnDate :: Parser SvnDate
svnDate = SvnDate
   <$> (count 4 digit >>= toDigit)  
   <*> (anyChar >> count 2 digit >>= toDigit)    
   <*> (anyChar >> count 2 digit >>= toDigit)   
   <*> (spaces >> count 2 digit >>= toDigit) 
   <*> (anyChar >> count 2 digit >>= toDigit)   
   <*> (count 11 anyChar >> count 3 letter) 
   <* count 14 anyChar
   where
   	toDigit = return . read
	

svnEntry ::  Parser SvnEntry
svnEntry = SvnEntry 
	<$> (manyTill anyChar (try brakeline)) 
	<*> (many (oneOf "-\n\rr") >> many1 digit >>= (return . read)) 
	<*> (separator >> many1 alphaNum) 
	<*> (separator >> svnDate)		
 	<*> (skipSomePart >> manyTill svnChanges ( choice [try newnewline, try myEof]))   
 	where
 		separator = string " | "
 		newnewline = string "\r\n\r\n"
 		skipSomePart = manyTill anyChar (try (string "  "))
 		myEof = (eof >>= \xx -> return "eof") :: Parser String
 		brakeline = string "----------"


uniqueCommiters :: [SvnEntry] -> [String]
uniqueCommiters xs = Set.toList $ foldl (\acc x -> (Set.insert (getName x) acc)) Set.empty xs

totalCommits :: (String -> SvnEntry -> Bool) -> [SvnEntry] -> String -> (String, Int)
totalCommits f xs p = (p, length $ filter (f p) xs)
	
nameCommitFilter :: String -> SvnEntry -> Bool
nameCommitFilter name x = (==) name $ getName x

timeCommitFilter :: String -> SvnEntry -> Bool
timeCommitFilter name x = (name == getName x) && (21 < hour x || 6 > hour x) 
	where hour = getHour . getDate

weekendCommitFilter :: String -> SvnEntry -> Bool
weekendCommitFilter name x = (name == getName x) && ("Sat" == day x || "Sun" == day x) 
	where day = getDayName . getDate

printStats :: [SvnEntry] -> IO ()
printStats xs = do
	let commiters = uniqueCommiters xs
	print "Weekend commits:" 
	print $ moreThen0Filter $ weekendCommits xs commiters
	print "Total commits:" 
	print $ overallCommits xs commiters
	print "Commits between 22:00 and 05:00" 
	print $ moreThen0Filter $ nightCommits xs commiters
	where
		moreThen0Filter = filter (\(n, i) -> i > 0)
		weekendCommits xs = map (totalCommits weekendCommitFilter xs) 
		overallCommits xs = map (totalCommits nameCommitFilter xs) 
		nightCommits xs = map (totalCommits timeCommitFilter xs) 

top15ChangedClasses :: [SvnEntry] -> IO ()
top15ChangedClasses xs = do
	print "Top 15 changed java classes: "
	let top15 = (take 15 . sortByFrequency . filterJava . pathHistogram . allPaths) xs 
	sequence_ $ fmap print top15
	where 
		allPaths = (map getPath) . join . (map getSvnActions)
		pathHistogram = MultiSet.toOccurList . MultiSet.fromList
		filterJava = filter (\(p, _) -> isInfixOf ".java" p)
		sortByFrequency = sortBy (\(_, f1) (_, f2) -> compare f2 f1)

updateMap map actions = foldl (\(xx,m) a -> (Map.insertLookupWithKey myInsert a 1 m)) map actions
	where myInsert key new old = new + old 


main :: IO ()
main = do
	s <- readFile "svn_test.log"
	case parse (many svnEntry) "test" s of
		(Right xs) -> printStats xs >> top15ChangedClasses xs 
		(Left b) -> print b
