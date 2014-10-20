import Prelude hiding (round)
import Data.List
import Data.List.Split
import Control.Applicative
import Control.Monad.Trans.Cont
import Data.Maybe 
import Control.Monad.Reader
import Control.Monad.Writer
import System.Random

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

data PlayAction = Fold | Call | Raise Money deriving (Eq, Show, Ord)
data RoundStartAction = Check | Bet Money | Fold_ deriving (Eq, Show, Ord)

data PokerBot = PokerBot { name :: String, startAction :: PokerAction RoundStartAction,  playAction :: PokerAction PlayAction } 
data BotState = BotState {hole :: Hand, moneyLeft :: Money, investedInPot :: Money, callNeeded :: Money, pot :: Money, communityCards :: CommunityCards} deriving (Show)
data TexasHoldemPoker = TexasHoldemPoker { bots :: [(PokerBot, BotState)], deck :: Deck}  deriving (Show)

type PokerAction a = ReaderT BotState IO a
type GamePlay a = WriterT String IO a 

pokerBot :: String -> PokerAction RoundStartAction -> PokerAction PlayAction -> PokerBot
pokerBot n r p = PokerBot { name = n, startAction = r, playAction = p}

evalHand :: BotState -> [HandEvaluation]
evalHand = undefined

playBot :: PokerAction PlayAction
playBot = do
	cards <- ask
	return Fold

playStart :: PokerAction RoundStartAction
playStart = do
	cards <- ask
	return Fold_

instance Show PokerBot where
  show b = "Bot: " ++ name b

class PokerGame a where 
	initGame :: [PokerBot] -> GamePlay a
	round ::  a -> GamePlay a 
	playGame :: a -> GamePlay a 
	
instance PokerGame TexasHoldemPoker where
	initGame = initGame_
	round = round_
	playGame = playGame_ 

folderBot :: PokerBot
folderBot = pokerBot "folder" (return Fold_) (return Fold)

round_ :: TexasHoldemPoker -> GamePlay TexasHoldemPoker
round_ x = do
	tell "removing deck"
	return (x {deck = []}) 

playGame_ :: TexasHoldemPoker -> GamePlay TexasHoldemPoker
playGame_ x = do
	tell $ "received " ++ (show $ (length . bots ) x) ++ " bots\n"
	y <- round_ x
	return y
	

initGame_ :: [PokerBot] -> GamePlay TexasHoldemPoker
initGame_ bs = writer (TexasHoldemPoker { bots = bz, deck = drop cardDealtLength theDeck}, "Created " ++ show (length bz) ++ " bots\n" )
	where 
		cardDealtLength = 2 * length bs
		botCards = chunksOf 2 $ take cardDealtLength theDeck
		bz = zipWith makeBots bs botCards
		makeBots b c = (b, s c)
		s c = BotState {hole = c, moneyLeft = 100, investedInPot = 0, callNeeded = 0, pot = 0, communityCards = []} 


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

runGame :: IO (TexasHoldemPoker, String)
runGame = runWriterT $ initGame [folderBot, folderBot] >>= playGame  


main = do
	x <- runGame
	putStrLn $ snd x
	--print runGame


--class (Functor m, Applicative m, MonadIO m) => RestrictedIO m where
--	rand :: m Double
	
--instance RestrictedIO IO where
--	rand = randomRIO (0.0, 1.0)


--ttest2 :: IO Double
--ttest2 = do 
--	print "this shouldnt happen"
--	randomRIO (0.0, 1.0)


--ttest :: RestrictedIO r => r Double
--ttest = do 
--	rand

--type PokerAction2 a b = ReaderT BotState a b

--tttest :: (RestrictedIO r) => PokerAction2 r PlayAction
--tttest = do
--	xx <- liftIO ttest
--	return $ Raise $ (floor $ xx * 100)

--dummyBotState = BotState {hole = [], moneyLeft = 100, investedInPot = 100, callNeeded = 100, pot = 100, communityCards = []} 