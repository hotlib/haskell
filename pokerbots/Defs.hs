module Defs where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Lens hiding (Fold, folded)

data PlayAction = Fold | Call | Raise Money deriving (Eq, Show, Ord)
data RoundStartAction = Check | Bet Money | Fold_ deriving (Eq, Show, Ord)

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


type PokerAction a = ReaderT BotState IO a
type GamePlay a = WriterT String IO a 

data PokerBot = PokerBot { _name :: String, _startAction :: PokerAction RoundStartAction,  _playAction :: PokerAction PlayAction, _folded :: Bool, _currentCall :: Money} 
data BotState = BotState {_hole :: Hand, _moneyLeft :: Int, _investedInPot :: Int, _callNeeded :: Int, _potTotal :: Int, _communityCards :: CommunityCards} -- deriving (Show)

data TexasHoldemPoker = TexasHoldemPoker { _bots :: [(PokerBot, BotState)], _deck :: Deck}  deriving (Show)

instance Show PokerBot where
  show b = "POKERBOT (folded: " ++ (show $ _folded b) ++ ", call: " ++ (show $ _currentCall b)  ++ ") " ++ _name b

instance Show BotState where
  show b = "BOTSTATE investedInPot: " ++ (show $ _investedInPot b) ++ " _moneyLeft: " ++ (show $ _moneyLeft b) ++ " _callNeeded: " ++ (show $ _callNeeded b)

type CompleteBot = (PokerBot, BotState)
type Pot = (Money, [CompleteBot])
type Pot2 = [(Money, CompleteBot)]

--instance Show Pot where
--  show p = "Pot money: " ++ fst p ++ " bots: " ++ show $ snd p

makeLenses ''PokerBot
makeLenses ''BotState
makeLenses ''TexasHoldemPoker

instance Ord PokerBot where
    compare x y
        | x^.name == y^.name    =  EQ
        | x^.name <= y^.name    =  LT
        | otherwise =  GT

instance Ord BotState where
	compare x y
         | x^.moneyLeft == y^.moneyLeft    =  EQ
         | x^.moneyLeft <= y^.moneyLeft    =  LT
         | otherwise =  GT

instance Eq BotState where
	(==) x y = x^.moneyLeft == y^.moneyLeft

instance Eq PokerBot where
	(==) x y = x^.name == y^.name

class PokerGame a where 
	initGame :: [PokerBot] -> GamePlay a
	round ::  a -> GamePlay a 
	playGame :: a -> GamePlay a 
	

