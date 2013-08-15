
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time (UTCTime, getCurrentTime)

instance Eq MyDate where
MyDate (x,y,z) == MyDate (a,b,c) = x Prelude.== a && b Prelude.== y && c Prelude.== z

parseDate :: String -> MyDate
parseDate x = let y:m:d:xs = map Prelude.read $ words x 
	   in MyDate (y, fromIntegral m, fromIntegral d)


newtype MyDate = MyDate (Integer, Int, Int) deriving Show


testParseDate :: String -> MyDate -> Bool
testParseDate s d = d Main.== parseDate s


currentDate :: IO MyDate
currentDate = getCurrentTime >>= return . MyDate . toGregorian . utctDay

isSelectedDate :: MyDate -> String -> Bool
isSelectedDate d s = d Main.== parseDate s

main = do 
	-- putStrLn "Started (current 09:00): "
	-- start <- getLine
	-- putStrLn "Finished (current 17:00): "
	-- finished <- getLine
	-- putStrLn "Break length in minutes (current 20): "
	-- finished <- getLine
	
	date <- currentDate
	print date


	s <- readFile "test.txt"
	
	let a = filter (isSelectedDate date) $ lines s
	
	-- date <- getCurrentTime >>= return . toGregorian . utctDay
	mapM_ putStrLn a

	-- putStrLn . show $ testParseDate "2013 8 15" $ MyDate date
	-- secondsInDay <- getCurrentTime >>= return . init . show . utctDayTime
	-- putStrLn secondsInDay

	
