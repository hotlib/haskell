
import Data.List
import Control.Applicative
import Control.Monad.Trans.Cont
import Data.Maybe 

data Suit = Club | Diamond | Heart | Spade deriving (Eq, Show, Enum, Ord)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace  deriving (Eq, Show, Enum, Ord)

type Card = (Suit, Value)

type Deck = [Card]
type Hand = [Card]

data CardEvaluation = HighCard Value Value Value Value Value 
					| OnePair Value Value Value Value 
					| TwoPair Value Value Value
					| ThreeOfAKind Value Value Value 
					| Straight Value Value Value Value Value 
					| Flush Value Value Value Value Value
					| FullHouse Value Value 
					| FourOfAKind Value Value 
					| StraightFlush Value deriving (Eq, Show, Ord)

theDeck :: Deck
theDeck = [(x, y) | x <- [Club .. Spade], y <- [Two .. Ace]]

isSorted :: (Ord a) => [a] -> Bool
isSorted xs = all (\(x, y) -> x <= y) $ zip xs (tail xs)

cardCombinations :: Int -> Deck -> [Hand]
cardCombinations n d = [ x | x <- mapM (const d) [1..n], (length . nub) x == n, isSorted x ] 

groupFreq :: Hand -> [Int]
groupFreq h = map snd $ freq h

fourOfAKind :: Hand -> Maybe CardEvaluation
fourOfAKind h 
	| 4 `elem` (groupFreq h) = let [(val1, _), (val2, _)] = freq h in Just $ FourOfAKind val1 val2  
	| otherwise = Nothing

fullHouse :: Hand -> Maybe CardEvaluation
fullHouse h  
	| isPrefixOf [3,2] (groupFreq h) = let [(val1, _), (val2, _)] = freq h in Just $ FullHouse val1 val2  
	| otherwise = Nothing

threeOfAKind :: Hand -> Maybe CardEvaluation
threeOfAKind h  
	| 3 `elem` (groupFreq h) = let [(val1, _), (val2, _), (val3, _)] = freq h in Just $ ThreeOfAKind val1 val2 val3 
	| otherwise = Nothing

twoPairs :: Hand -> Maybe CardEvaluation
twoPairs h  
	| isPrefixOf [2,2] (groupFreq h) = let [(val1, _), (val2, _), (val3, _)] = freq h in Just $ TwoPair val1 val2 val3 
	| otherwise = Nothing

onePair :: Hand -> Maybe CardEvaluation
onePair h  
	| 2 `elem` (groupFreq h) = let [(val1, _), (val2, _), (val3, _), (val4, _)] = freq h in Just $ OnePair val1 val2 val3 val4
	| otherwise = Nothing

highCard :: Hand -> Maybe CardEvaluation
highCard h  = let [(val1, _), (val2, _), (val3, _), (val4, _), (val5, _)] = freq h in Just $ HighCard val1 val2 val3 val4 val5

straight :: Hand -> Maybe CardEvaluation
straight h 
	| isInfixOf valuesHand allValuesSorted = let [val1, val2, val3, val4, val5] = reverse valuesHand in Just $ Straight val1 val2 val3 val4 val5 -- TODO how to do it more haskell-y?
	| otherwise = Nothing
	where 
		allValuesSorted = [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]
		valuesHand = sort $ map snd h

straightFlush :: Hand -> Maybe CardEvaluation
straightFlush h 
	| sameSuit h = straight h
	| otherwise = Nothing

sameSuit :: Hand -> Bool
sameSuit h = all (== head values) values
	where values = map fst h

flush :: Hand -> Maybe CardEvaluation
flush h
	| sameSuit h = let [val1, val2, val3, val4, val5] = reverse . sort $ map snd h in Just $ Flush val1 val2 val3 val4 val5 
	| otherwise = Nothing

eval :: (Hand -> Maybe CardEvaluation) -> Hand -> Cont (Maybe CardEvaluation) (Maybe CardEvaluation)
eval f h = let r = f h in 
		if isJust r 
		then cont $ \_ -> r 
		else cont $ \k -> k r 
	
evaluateHand :: Hand -> Cont (Maybe CardEvaluation)  (Maybe CardEvaluation) 
evaluateHand h = 
	do
		eval straightFlush h 
		eval fourOfAKind h
		eval fullHouse h
		eval flush h
		eval straight h
		eval threeOfAKind h
		eval twoPairs h
		eval onePair h
		eval highCard h

freq :: Hand -> [(Value, Int)]
freq h = sortBy f $ map (\x -> (head x, length x)) . group . sort $ values
	where 
		values = map snd h
		f (v1, i) (v2,j)  
			| compare j i == EQ = compare v2 v1
			| otherwise = compare j i
			

testHand :: Hand
testHand = [(Club,Six),(Club,Two), (Club,Six),(Club,Four),(Club,Two)]


testHand2 :: Hand
testHand2 = [(Club,Five),(Club,Three), (Club,Six),(Club,Four),(Club,Two)]

main = do 
	print $ runCont (evaluateHand testHand) id
	-- print . length . cardCombinations 5 $ take 7 theDeck
