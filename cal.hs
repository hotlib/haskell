import Data.Time.Clock
import Data.Time.Calendar
import Data.Time (UTCTime, getCurrentTime)

newtype MyDate = MyDate (Integer, Int, Int) deriving Show

instance Eq MyDate where
MyDate x == MyDate y = x Prelude.== y

parseDate :: String -> MyDate
parseDate x = let y:m:d:xs = map Prelude.read $ words x 
	   in MyDate (y, fromIntegral m, fromIntegral d)

currentDate :: IO MyDate
currentDate = getCurrentTime >>= return . MyDate . toGregorian . utctDay

isSelectedDate :: MyDate -> String -> Bool
isSelectedDate d s = d Main.== parseDate s

getNewValue :: String -> IO String
getNewValue oldValue = do 
				newValue <- getLine
				if null newValue
  					then return oldValue
  					else return newValue	

main = do 
	date <- currentDate
	s <- readFile "test.txt"
	
	let rec:recs = filter (isSelectedDate date) $ lines s
	
	let break:finish:start:rest = reverse . words $ rec

	putStrLn $ "Started (current " ++ start ++ "): "
	newStart <- getNewValue start
	putStrLn $ "Finished (current " ++ finish ++ "): "
	newFinished <- getNewValue finish
	putStrLn $ "Break length in minutes (current " ++ break ++ "): "
	newBreak <- getNewValue break

	print $ filter (not . (isSelectedDate date)) $ lines s