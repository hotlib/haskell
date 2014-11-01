module Evaluation where

import Data.List
import Data.List.Split
import Control.Monad.Trans.Cont
import Data.Maybe 
import Defs

freq :: Hand -> [(Value, Int)]
freq h = sortBy f $ map (\x -> (head x, length x)) . group . sort $ values
	where 
		values = map snd h
		f (v1, i) (v2,j)  
			| compare j i == EQ = compare v2 v1
			| otherwise = compare j i

theDeck :: Deck
theDeck = [(x, y) | x <- [Club .. Spade], y <- [Two .. Ace]]

isSorted :: (Ord a) => [a] -> Bool
isSorted xs = all (\(x, y) -> x <= y) $ zip xs (tail xs)

cardCombinations :: Int -> Deck -> [Hand]
cardCombinations n d = [ x | x <- mapM (const d) [1..n], (length . nub) x == n, isSorted x ] 

groupFreq :: Hand -> [Int]
groupFreq h = map snd $ freq h

fourOfAKind :: Hand -> Maybe HandEvaluation
fourOfAKind h 
	| 4 `elem` (groupFreq h) = Just . FourOfAKind $ map fst $ freq h  
	| otherwise = Nothing

fullHouse :: Hand -> Maybe HandEvaluation
fullHouse h  
	| isPrefixOf [3,2] (groupFreq h) = Just . FullHouse $ map fst $ freq h  
	| otherwise = Nothing

threeOfAKind :: Hand -> Maybe HandEvaluation
threeOfAKind h  
	| 3 `elem` (groupFreq h) = Just . ThreeOfAKind $ map fst $ freq h  
	| otherwise = Nothing

twoPairs :: Hand -> Maybe HandEvaluation
twoPairs h  
	| isPrefixOf [2,2] (groupFreq h) = Just . TwoPair $ map fst $ freq h 
	| otherwise = Nothing

onePair :: Hand -> Maybe HandEvaluation
onePair h  
	| 2 `elem` (groupFreq h) = Just . OnePair $ map fst $ freq h
	| otherwise = Nothing

highCard :: Hand -> Maybe HandEvaluation
highCard h  = Just . HighCard . reverse . sort $ map snd h

straight :: Hand -> Maybe HandEvaluation
straight h 
	| isInfixOf valuesHand allValuesSorted = Just . Straight $ reverse valuesHand
	| otherwise = Nothing
	where 
		allValuesSorted = [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]
		valuesHand = sort $ map snd h

straightFlush :: Hand -> Maybe HandEvaluation
straightFlush h 
	| sameSuit h = straight h
	| otherwise = Nothing

sameSuit :: Hand -> Bool
sameSuit h = all (== head values) values
	where values = map fst h

flush :: Hand -> Maybe HandEvaluation
flush h
	| sameSuit h = Just . Flush $ reverse . sort $ map snd h 
	| otherwise = Nothing

eval :: (Hand -> Maybe HandEvaluation) -> Hand -> Cont (Maybe HandEvaluation) (Maybe HandEvaluation)
eval f h = let r = f h in 
		if isJust r 
		then cont $ \_ -> r 
		else cont $ \k -> k r 
	
evaluateHand :: Hand -> Cont (Maybe HandEvaluation)  (Maybe HandEvaluation) 
evaluateHand h = do
	eval straightFlush h 
	eval fourOfAKind h
	eval fullHouse h
	eval flush h
	eval straight h
	eval threeOfAKind h
	eval twoPairs h
	eval onePair h
	eval highCard h