import Data.Time.Clock
import Data.Time.Calendar
import Data.Time (UTCTime, getCurrentTime)

newtype MyDate = MyDate (Integer, Int, Int) 

instance Show MyDate where
    show (MyDate (y, m, d))   = show y  ++ " " ++ show m ++ " " ++ show d
    
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

formatDates :: [String] -> String
formatDates s = foldl1 (\a c -> c ++ "\n" ++ a) s

dateToString :: MyDate -> String -> String -> String -> String
dateToString date start finish break =
	show date ++ " " ++ start  ++ " " ++ finish ++ " " ++ break ++ " \n"

defaultRecord :: MyDate -> String 
defaultRecord d = show d ++ " 09:00 " ++ " 17:20 " ++ " 20 "

main = do 
	d <- currentDate
	c <- readFile "test.txt"
	let record = currentRecord d c
	
	let break:finish:start:_ = reverse . words $ record

	putStrLn $ "Current date " ++ (show d) ++ " (YYYY MM DD): "
	nd <- getNewValue $ show d
	let newDate = parseDate nd
	putStrLn $ "Started (current " ++ start ++ "): "
	newStart <- getNewValue start
	putStrLn $ "Finished (current " ++ finish ++ "): "
	newFinished <- getNewValue finish
	putStrLn $ "Break length in minutes (current " ++ break ++ "): "
	newBreak <- getNewValue break

	let newRec = dateToString newDate newStart newFinished newBreak
	writeFile "test2.txt" $ formatDates $ newRec : (filter (not . (isSelectedDate newDate)) $ lines c)
	where currentRecord date fileContent = let filtered = filter (isSelectedDate date) $ lines fileContent
										   in if null filtered
										   		then defaultRecord date
										   		else head filtered 
	