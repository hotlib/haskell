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
import Plumbing

instance PokerGame TexasHoldemPoker where
	initGame = initGame_
	round = round_
	playGame = playGame_ 

pokerBot :: String -> PokerAction RoundStartAction -> PokerAction PlayAction -> PokerBot
pokerBot n r p = PokerBot { _name = n, _startAction = r, _playAction = p, _botStatus = Playing, _currentCall = 0}

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

updateState f1 f2 = (moneyLeft %~ f1) . (investedInPot %~ f2)

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

helper :: Money -> Money -> Money -> CompleteBot -> CompleteBot
helper callOrRaise currentInv money b = 
	if money <= callOrRaise then
		setAllIn . (us callOrRaise currentInv money) $ b
	else
		(us callOrRaise currentInv callOrRaise) b
  where
  	us c i m (p,s)  = (p&currentCall .~ (c + i), updateState (subtract m) (+ m) s)

updateBotState :: PlayAction -> CompleteBot -> CompleteBot
updateBotState a b = 
	case a of 
		Fold -> setFolded b
		Call -> helper call inv money b
		(Raise m) -> helper (m + call) inv money b
	where
		money = moneyleft b
		call = callneeded b
		inv = invested b	


normalRound :: TexasHoldemPoker -> GamePlay TexasHoldemPoker
normalRound t = iterateUntilM (\thp -> finishedBetting $ thp^.bots) normalRound2 t

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
		updatedBots = foldl (\iobs b -> iobs >>= (updater b normalizedBots)) (return []) normalizedBots
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

finishedBetting :: [CompleteBot] -> Bool
finishedBetting bs = everyoneAllIn || everyoneFolded || everyoneBetTheSame   
	where 
		maxBet = maximum $ map invested playingBots 
		everyoneAllIn = all isAllIn playingBots
		everyoneFolded = null playingBots
		everyoneBetTheSame = all (\b -> maxBet == invested b || (maxBet > invested b && isAllIn b)) playingBots  
		playingBots = filter (not . hasFolded) bs

restBot :: PlayAction -> CompleteBot -> [CompleteBot] -> IO [CompleteBot]
restBot Fold b bs = return $ (setFolded b) : bs
restBot _ b bs = return $ b : bs

updater :: CompleteBot -> [CompleteBot] -> [CompleteBot] -> IO [CompleteBot]
updater b@(p,s) oldBs [] 
	| notPlaying b = return [b] 
	| otherwise = firstBot $ (p, s_)
	where s_ = s&potTotal .~ (splitPots oldBs)
updater (p,s) oldBs bs@(b1:_)  
	| notPlaying b = return $ b : bs
	| otherwise = do
		 action <- playBot b
		 print $ "----->>>>> action " ++ (show action) ++ " Bot: " ++ (show $ updateBotState action b) -- ++ " invB: " ++ (show invB) ++ " invB1 " ++ (show invB1)
		 let newBot = updateBotState action b
		  in restBot action newBot bs  		 
	where
		b = (p&currentCall .~ (currentcall b1) ,s& ((callNeeded .~ needed) . (potTotal .~ splitPots mergedBots)))
		needed = (currentcall b1) - (invested b)
		mergedBots = let x = filter (\b -> not $ isBotInList b bs) oldBs in x ++ bs
		pots = splitPots mergedBots

playGame_ :: TexasHoldemPoker -> GamePlay TexasHoldemPoker
playGame_ x = do
	tell $ "received " ++ (show $ (length . _bots ) x) ++ " bots\n"
	y <- round_ x
	return y

initGame_ :: [PokerBot] -> GamePlay TexasHoldemPoker
initGame_ bs = writer (TexasHoldemPoker { _bots = bz, _deck = drop cardDealtLength theDeck}, "Created " ++ show (length bz) ++ " bots\n" )
	where 
		cardDealtLength = 2 * length bs
		botCards = chunksOf 2 $ take cardDealtLength theDeck
		bz = zipWith makeBots bs botCards
		makeBots b c = (b, botState c)

testHand :: Hand
testHand = [(Club,Six),(Club,Two), (Club,Six),(Club,Four),(Club,Two)]

runGame :: (PokerGame g) => IO (g, String)
runGame = runWriterT $ initGame dummyBots2 >>= playGame  


splitPots :: [CompleteBot] -> [PotSimple]
splitPots = splitPots3 [] 

splitPots3 :: [PotSimple] -> [CompleteBot] -> [PotSimple]
splitPots3 pots [] = pots
splitPots3 pots bs = splitPots3 updatePot otherBots 
    where
       currentBotInvest = map simpleBot updatedBot
       updatedBot = map (\(p,s)-> (p, s&investedInPot %~ (subtract minimumInvestment)))  bs
       minimumInvestment = minimum $ map invested bs 
       otherBots = filter (\b -> 0 < invested b) updatedBot
       createPot = PotSimple { _bet = (minimumInvestment * length currentBotInvest), _botss = currentBotInvest}
       updatePot = createPot : pots
       decrInvestment (p,s) = (p, s&moneyLeft %~ (subtract minimumInvestment)) -- not necessary

isBotInList :: CompleteBot -> [CompleteBot] -> Bool
isBotInList (p,s) list = any (\(p1,_) -> p1^.name == p^.name ) list

collectBots :: [CompleteBot] -> [Pot2] -> [CompleteBot]
collectBots foldedBots pots = foldl (\list (_, bot@(p,s)) -> if isBotInList bot list then list else (p,s&investedInPot .~ 0) : list) foldedBots $ concat pots
   
dummyBots = [(folderBot "x", (botState [])&investedInPot .~ 23), 
	(folderBot "xx", (botState [])&investedInPot .~ 11), 
	(folderBot "xxx", (botState [])&investedInPot .~ 222),
	(folderBot "xxxx", (botState [])&investedInPot .~ 11)] 

   
defaultMain = do
	x <- runGame :: IO (TexasHoldemPoker, String)
	putStrLn $ snd x
	print $ fst x
	
	-- x <- playBot folderBot (botState [])
	-- print x
	-- print $ splitPots3 [] dummyBots
	-- print "ok"