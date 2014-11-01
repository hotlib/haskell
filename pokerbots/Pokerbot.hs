module Pokerbot where

import Prelude hiding (round)
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Lens hiding (Fold, folded)
import Data.List.Split
import Control.Monad.Trans.Cont
import Data.Maybe 
import Control.Monad.Loops
import Control.Applicative
import Defs
import Evaluation 

instance PokerGame TexasHoldemPoker where
	initGame = initGame_
	round = round_
	playGame = playGame_ 

pokerBot :: String -> PokerAction RoundStartAction -> PokerAction PlayAction -> PokerBot
pokerBot n r p = PokerBot { _name = n, _startAction = r, _playAction = p, _folded = False, _currentCall = 0}

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
bigBlindBet t = return $ t&bots %~ \(b:bs) -> bs ++ [(fst b, (invest bblind $ snd b))]

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
updateBotState a b@(p, s) = 
	case a of 
		Fold -> b
		Call -> (p&currentCall .~ (call + inv), updateState (subtract call) (+ call) s)
		(Raise m) -> let raise = m + call in (p&currentCall .~ (raise + inv), updateState (subtract raise) (+ raise) s)
	where
		call = callneeded b
		inv = invested b	


wrapperRound :: TexasHoldemPoker -> GamePlay TexasHoldemPoker
wrapperRound t = undefined

normalRound :: TexasHoldemPoker -> GamePlay TexasHoldemPoker
normalRound t = iterateUntilM (\thp -> continueBetting $ thp^.bots) normalRound2 t

normaliseCalls :: [CompleteBot] -> [CompleteBot]
normaliseCalls bs = map normalise bs 
	where 
		normalise b@(p,s) = (p&currentCall .~ call, s&callNeeded .~ (call - (invested b))) 
		bots = filter (not . hasFolded) bs
		call = maximum $ map currentcall bs

normalRound2 :: TexasHoldemPoker -> GamePlay TexasHoldemPoker
normalRound2 t = do
	liftIO $ print "-----------NOT UPDATED--------------------------"
	liftIO $ print $ t^.bots
	liftIO $ print "-----------NOT UPDATED END----------------------"
	bs <- liftIO $ (normaliseCalls . reverse) <$> updatedBots
	liftIO $ print "---------------UPDATED--------------------------"
	liftIO $ print bs
	liftIO $ print "---------------UPDATED END----------------------"
	return $ t&bots .~ bs
	where 
		updatedBots = foldl (\x y -> x >>= (updater y)) (return []) normalizedBots
		normalizedBots = normaliseCalls $ t^.bots
 	
dummyBots2 = [callBot "xx",  
			  folderBot "xxxx",
			  raiseBot "xxx",
			  folderBot "x"] 

firstBot :: CompleteBot -> IO [CompleteBot]
firstBot b = do 
		action <- playBot b
		print $ "----->>>>> action (first) " ++ (show action) ++ " Bot: " ++ (show $ updateBotState action b)
		let newBot = updateBotState action b 
		 in 
		 if action /= Fold then 
		 	return [newBot]
		 else
		 	return [setFolded newBot]
	
hasFolded :: CompleteBot -> Bool
hasFolded b = (fst b)^.folded

setFolded :: CompleteBot -> CompleteBot
setFolded b = ((fst b)&folded .~ True, snd b)

invested :: CompleteBot -> Money
invested = ((^.investedInPot) . snd) 

callneeded :: CompleteBot -> Money
callneeded = ((^.callNeeded) . snd)

currentcall :: CompleteBot -> Money
currentcall = ((^.currentCall) . fst)

continueBetting :: [CompleteBot] -> Bool
continueBetting bs = all (\b -> (invested $ head bots) == invested b) bots 
	where bots = filter (not . hasFolded) bs

restBot :: PlayAction -> CompleteBot -> [CompleteBot] -> IO [CompleteBot]
restBot Fold b bs = return $ (setFolded b) : bs
restBot _ b bs = return $ b : bs

updater :: CompleteBot -> [CompleteBot] -> IO [CompleteBot]
updater b [] = do 
	if hasFolded b then
		return [b] 
	else
		firstBot b
updater (p,s) bs@(b1:_) = 
	if hasFolded b then
		return $ b : bs
	else
		do
		 action <- playBot b
		 print $ "----->>>>> action " ++ (show action) ++ " Bot: " ++ (show $ updateBotState action b) -- ++ " invB: " ++ (show invB) ++ " invB1 " ++ (show invB1)
		 let newBot = updateBotState action b
		  in restBot action newBot bs  		 
	where
    	b = (p&currentCall .~ (currentcall b1) ,s&callNeeded .~ needed)
    	needed = (currentcall b1) - (invested b)

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