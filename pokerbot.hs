{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import Data.List
import Control.Applicative
import Control.Monad.Trans.Cont
import Data.Maybe 
import Control.Monad.Reader
import Control.Monad.Writer

data Suit = Club | Diamond | Heart | Spade deriving (Eq, Show, Enum, Ord)
data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace  deriving (Eq, Show, Enum, Ord)
type Card = (Suit, Value)
data HandEvaluation = HighCard [Value] 
					| OnePair [Value] 
					| TwoPair [Value]
					| ThreeOfAKind [Value]
					| Straight [Value] 
					| Flush [Value]
					| FullHouse [Value] 
					| FourOfAKind [Value]
					| StraightFlush [Value] deriving (Eq, Show, Ord)

type Deck = [Card]
type Hand = [Card]
type CommunityCards = [Card]
type Money = Int 
data BotAction = Fold | Call | Raise Int 

data PokerBot = PokerBot { name :: String, runAction :: PokerAction } 
data BotState = BotState {hand :: Hand, moneyLeft :: Money, investedInPot :: Money, callNeeded :: Money, pot :: Money, communityCards :: CommunityCards} deriving (Show)
data TexasHoldemPoker = TexasHoldemPoker { bots :: [PokerBot], botStates :: [BotState], deck :: Deck}  deriving (Show)

type PokerAction = Reader BotState BotAction 
type GamePlay g = Writer String g 

evalHand :: BotState -> [HandEvaluation]
evalHand = undefined

playBot :: PokerAction
playBot = do
	cards <- ask
	return Fold

playGame2 :: [PokerBot] -> (GamePlay TexasHoldemPoker)
playGame2 bs = do	
	tell "starting game; "
	let gs = initGame []
	gs

instance Show PokerBot where
  show b = "Bot: " ++ name b

class PokerGame g where
	initGame :: [PokerBot] -> g
	round :: g -> g
	
instance PokerGame (GamePlay TexasHoldemPoker) where
	initGame bs = writer (TexasHoldemPoker { botStates = [],  bots = [], deck = []}, "converted bots")
	round = undefined

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

freq :: Hand -> [(Value, Int)]
freq h = sortBy f $ map (\x -> (head x, length x)) . group . sort $ values
	where 
		values = map snd h
		f (v1, i) (v2,j)  
			| compare j i == EQ = compare v2 v1
			| otherwise = compare j i
			

testHand :: Hand
testHand = [(Club,Six),(Club,Two), (Club,Six),(Club,Four),(Club,Two)]

transform :: Hand -> HandEvaluation
transform h = fromJust $ runCont (evaluateHand h) id

main = do 
	print "test"
	-- print $ hand . head $ initBots someMetadata
	--- print . length . cardCombinations 5 $ take 7 theDeck
	-- print $ map transform $ cardCombinations 5 $ take 12 theDeck 

--playGame :: (PokerGame g) => [PokerBot] -> g
--playGame bs = do
--	initBots bs
--instance PokerGame TexasHoldemPoker where
--	initBots bs = TexasHoldemPoker { gameState = Table {pot = 100, communityCards = []}, bots = []}
--	round = undefined
--	evalCards = undefined
--	result = undefined
