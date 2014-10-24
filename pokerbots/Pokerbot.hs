module Pokerbot where

import Prelude hiding (round)
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Lens hiding (Fold)
import Evaluation 
import Data.List.Split
import Control.Monad.Trans.Cont
import Data.Maybe 

data PlayAction = Fold | Call | Raise Money deriving (Eq, Show, Ord)
data RoundStartAction = Check | Bet Money | Fold_ deriving (Eq, Show, Ord)

type PokerAction a = ReaderT BotState IO a
type GamePlay a = WriterT String IO a 

data PokerBot = PokerBot { _name :: String, _startAction :: PokerAction RoundStartAction,  _playAction :: PokerAction PlayAction } 
data BotState = BotState {_hole :: Hand, _moneyLeft :: Int, _investedInPot :: Int, _callNeeded :: Int, _pot :: Int, _communityCards :: CommunityCards} deriving (Show)
data TexasHoldemPoker = TexasHoldemPoker { _bots :: [(PokerBot, BotState)], _deck :: Deck, _startingBot :: Int}  deriving (Show)

instance Show PokerBot where
  show b = "Bot: " ++ _name b

makeLenses ''PokerBot
makeLenses ''BotState
makeLenses ''TexasHoldemPoker

pokerBot :: String -> PokerAction RoundStartAction -> PokerAction PlayAction -> PokerBot
pokerBot n r p = PokerBot { _name = n, _startAction = r, _playAction = p}

playBot :: PokerBot -> BotState -> IO PlayAction
playBot b s = runReaderT action s
	where
		action = b^.playAction 

playRoundStartBot :: PokerBot -> BotState -> IO RoundStartAction
playRoundStartBot b s = runReaderT action s
	where
		action = b^.startAction 

evalHand :: BotState -> [HandEvaluation]
evalHand = undefined

playBotExample :: PokerAction PlayAction
playBotExample = do
	cards <- ask
	return Fold

playStartExample :: PokerAction RoundStartAction
playStartExample = do
	cards <- ask
	return Fold_


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

invest ::  Money -> BotState -> BotState
invest m b = let 
	decr = (subtract m) 
	incr = (+ m)
	updateState = (moneyLeft %~ decr) . (pot %~ incr) . (investedInPot %~ incr)
	in
	b & updateState

smallBlindBet :: TexasHoldemPoker -> GamePlay TexasHoldemPoker
smallBlindBet t = return $ t&bots %~ \(b:bs) -> bs ++ [(fst b, invest 5 $ snd b)]

bigBlindBet :: TexasHoldemPoker -> GamePlay TexasHoldemPoker
bigBlindBet t = return $ t&bots %~ \(b:bs) -> (fst b, (invest 10 $ snd b)) : bs  

rround :: Money -> [(PokerBot, BotState)] -> [(PokerBot, BotState)]
rround = undefined -- foldl id 

bet1 :: TexasHoldemPoker -> GamePlay TexasHoldemPoker
bet1 t = let 
		b:bs = t^.bots
		bs2 = rround 22 bs 
	in  
	return $ (t&bots .~ b:bs2)  

round_ :: TexasHoldemPoker -> GamePlay TexasHoldemPoker
round_ t = smallBlindBet t >>= bigBlindBet -- >>= bet1



		--return $ t&bots %~ \(b:bs) -> (fst b, (bigBlind $ snd b)) : bs  
-- t&startingBot %~ (+1)
		-- return t
	-- let 
	--	bz = t^.bots
		-- smallBlindBot = ((bz !! 0)^._2) -- (t^.startingBot `mod` (length bz))
	--	in
		-- return t
	-- choose small and big blind and add to pot IFF pot empty
	-- otherwise RoundStartAction until someone bets
	-- afterwards normal PokerAction
	

playGame_ :: TexasHoldemPoker -> GamePlay TexasHoldemPoker
playGame_ x = do
	tell $ "received " ++ (show $ (length . _bots ) x) ++ " bots\n"
	y <- round_ x
	return y

botState :: [Card] -> BotState
botState c = BotState {_hole = c, _moneyLeft = 100, _investedInPot = 0, _callNeeded = 0, _pot = 0, _communityCards = []}	

initGame_ :: [PokerBot] -> GamePlay TexasHoldemPoker
initGame_ bs = writer (TexasHoldemPoker { _bots = bz, _deck = drop cardDealtLength theDeck, _startingBot = 0}, "Created " ++ show (length bz) ++ " bots\n" )
	where 
		cardDealtLength = 2 * length bs
		botCards = chunksOf 2 $ take cardDealtLength theDeck
		bz = zipWith makeBots bs botCards
		makeBots b c = (b, botState c)

testHand :: Hand
testHand = [(Club,Six),(Club,Two), (Club,Six),(Club,Four),(Club,Two)]

transform :: Hand -> HandEvaluation
transform h = fromJust $ runCont (evaluateHand h) id

runGame :: (PokerGame g) => IO (g, String)
runGame = runWriterT $ initGame [folderBot, folderBot] >>= playGame  

ppprint = print "ok"

defaultMain = do
	--x <- runGame :: IO (TexasHoldemPoker, String)
	--putStrLn $ snd x
	--print $ fst x
	x <- playBot folderBot (botState [])
	print x 