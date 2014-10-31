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
data BotState = BotState {_hole :: Hand, _moneyLeft :: Int, _investedInPot :: Int, _callNeeded :: Int, _potTotal :: Int, _communityCards :: CommunityCards} -- deriving (Show)

data TexasHoldemPoker = TexasHoldemPoker { _bots :: [(PokerBot, BotState)], _deck :: Deck}  deriving (Show)

instance Show PokerBot where
  show b = "POKERBOT: " ++ _name b

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

pokerBot :: String -> PokerAction RoundStartAction -> PokerAction PlayAction -> PokerBot
pokerBot n r p = PokerBot { _name = n, _startAction = r, _playAction = p}

playBot :: CompleteBot -> IO PlayAction
playBot (b, s) = runReaderT action s
	where
		action = b^.playAction 

playRoundStartBot :: CompleteBot -> IO RoundStartAction
playRoundStartBot (b, s) = runReaderT action s
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

folderBot :: String -> PokerBot
folderBot name = pokerBot name (return Fold_) (return Fold)

callBot :: String -> PokerBot
callBot name = pokerBot name (return Fold_) (return Call)

raiseBot :: String -> PokerBot
raiseBot name = pokerBot name (return Fold_) (return $ Raise 25)

updateState f1 f2 = (moneyLeft %~ f1) . (potTotal %~ f2) . (investedInPot %~ f2)

invest ::  Money -> BotState -> BotState
invest m b = let 
	decr = (subtract m) 
	incr = (+ m)
	update = updateState decr incr
	in
	b & update

bblind :: Money
bblind = 10

sblind :: Money
sblind = 5

smallBlindBet :: TexasHoldemPoker -> GamePlay TexasHoldemPoker
smallBlindBet t = return $ t&bots %~ \(b:bs) -> bs ++ [(fst b, invest sblind $ snd b)]

bigBlindBet :: TexasHoldemPoker -> GamePlay TexasHoldemPoker
bigBlindBet t = return $ t&bots %~ \(b:bs) -> (fst b, (invest bblind $ snd b)) : bs  

rround :: Money -> [(PokerBot, BotState)] -> [(PokerBot, BotState)]
rround = undefined -- foldl id 

bet1 :: TexasHoldemPoker -> GamePlay TexasHoldemPoker
bet1 t = let 
		b:bs = t^.bots
		bs2 = rround 22 bs 
	in  
	return $ (t&bots .~ b:bs2)  

round_ :: TexasHoldemPoker -> GamePlay TexasHoldemPoker
round_ t = smallBlindBet t >>= bigBlindBet >>= normalRound

updateBotState :: PlayAction -> CompleteBot -> CompleteBot
updateBotState a bot@(b, s) = 
	case a of 
		Fold -> bot
		Call -> (b, updateState (subtract call) (+ call) s)
		(Raise m) -> let amount = m + call in (b, updateState (subtract amount) (+ amount) s)
	where
		call = s^.callNeeded	

normalRound :: TexasHoldemPoker -> GamePlay TexasHoldemPoker
normalRound t = do
	liftIO $ print $ t^.bots
	liftIO $ print "-------------------------------------"
	bs <- liftIO updatedBots
	liftIO $ print bs
	return $ t&bots .~ bs
	where 
		updatedBots = foldl (\x y -> x >>= (updater y)) (return []) $ t^.bots
 	
dummyBots2 = [callBot "xx",  folderBot "xxxx",raiseBot "xxx", folderBot "x"] 

firstAction :: CompleteBot -> IO [CompleteBot]
firstAction b = do 
		action <- playBot b
		print $ "action FIRST " ++ (show action) ++ " Bot: " ++ (show b)
		if action /= Fold then
			let newBot = updateBotState action b in return [newBot]
		else
			return [] 

updater :: CompleteBot -> [CompleteBot] -> IO [CompleteBot]
updater b [] = do 
	if ((snd b)^.callNeeded) == 0 then
		return [b] -- he was the big blind
	else
		firstAction b
updater (p,s) bs@(b1:_) = do 
	action <- playBot b
	print $ "action " ++ (show action) ++ " Bot: " ++ (show b) ++ " invB: " ++ (show invB) ++ " invB1 " ++ (show invB1)
	if action /= Fold then
		let newBot = updateBotState action b
	 in return $ newBot : bs
	else
		return bs 	
    where
    	b = (p,s&callNeeded .~ newCall)
    	newCall = invB1 - invB
    	invB = invested b 
        invB1 = if (invested b1) < (callneeded b1) then (callneeded b1) else (invested b1) 
    	invested = ((^.investedInPot) . snd)
    	callneeded = ((^.callNeeded) . snd)

playGame_ :: TexasHoldemPoker -> GamePlay TexasHoldemPoker
playGame_ x = do
	tell $ "received " ++ (show $ (length . _bots ) x) ++ " bots\n"
	y <- round_ x
	return y

botState :: [Card] -> BotState
botState c = BotState {_hole = c, _moneyLeft = 100, _investedInPot = 0, _callNeeded = 0, _potTotal = 0, _communityCards = []}	

initGame_ :: [PokerBot] -> GamePlay TexasHoldemPoker
initGame_ bs = writer (TexasHoldemPoker { _bots = bz, _deck = drop cardDealtLength theDeck}, "Created " ++ show (length bz) ++ " bots\n" )
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
runGame = runWriterT $ initGame dummyBots2 >>= playGame  

splitPots :: [Pot] -> [CompleteBot] -> [Pot]
splitPots pots [] = pots
splitPots pots [_] = pots
splitPots pots bs = splitPots updatePot otherBots 
	where
		bsInvest = map (\(p,s) -> (s^.investedInPot, (p,s))) bs 
		minimumInvestment = minimum $ map potInvestment bs 
		-- minBots = filter (\(n, _) -> n == (fst . minimum) bsInvest) bsInvest
		otherBots = filter (\x -> minimumInvestment < potInvestment x) bs
		potSize = (fst . minimum $ bsInvest) * (fromIntegral (length bsInvest))
		updatePot = (potSize,bs) : pots
		potInvestment = ((^.investedInPot) . snd)

splitPots2 :: [Pot2] -> [CompleteBot] -> [Pot2]
splitPots2 pots [] = pots
splitPots2 (h:hs) [(p,s)] = let 
    h_ = filter (\(_, (p1,s1)) -> p1^.name /= p^.name) h 
    [(mi,_), _] = h
    returnMoney = (s^.investedInPot) - mi
    updatedBot = (mi, (p, s&moneyLeft %~ (+ returnMoney)))
    in
    (updatedBot : h_) : hs 
splitPots2 pots bs = splitPots2 updatePot otherBots 
    where
        currentPotInvest = map (\b -> (minimumInvestment, decrInvestment b)) bs 
        minimumInvestment = minimum $ map potInvestment bs 
        otherBots = map decrInvestment $ filter (\x -> minimumInvestment < potInvestment x) bs
        updatePot = currentPotInvest : pots
        potInvestment = ((^.investedInPot) . snd)
        decrInvestment (p,s) = (p, s&moneyLeft %~ (subtract minimumInvestment))

collectBots :: [CompleteBot] -> [Pot2] -> [CompleteBot]
collectBots foldedBots pots = foldl (\list (_, bot@(p,s)) -> if isBotInList bot list then list else (p,s&investedInPot .~ 0) : list) foldedBots $ concat pots
    where
        isBotInList (p,s) list = any (\(p1,_) -> p1^.name == p^.name ) list 

dummyBots = [(folderBot "x", (botState [])&investedInPot .~ 23), 
	(folderBot "xx", (botState [])&investedInPot .~ 11), 
	(folderBot "xxx", (botState [])&investedInPot .~ 222),
	(folderBot "xxxx", (botState [])&investedInPot .~ 11)] 

   
defaultMain = do
	x <- runGame :: IO (TexasHoldemPoker, String)
	putStrLn $ snd x
	print $ fst x
	 ----
	--x <- playBot folderBot (botState [])
	--print x
	-- print $ collectBots [] $ splitPots2 [] dummyBots
    -- print "ok"