module Pokerbot where

import Prelude hiding (round)
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Lens hiding (Fold, folded)
import Data.List.Split
import Control.Monad.Trans.Cont
import Data.Maybe 
import Defs
import Evaluation 
import Plumbing

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

playRoundStartBot2 :: CompleteBot -> IO RoundStartAction
playRoundStartBot2 b = 
	if notPlaying b then return Check else (playRoundStartBot b) 

evalRoundStart :: CompleteBot -> RoundStartAction -> IO CompleteBot
evalRoundStart b Fold_ = return $ updateBotState Fold b
evalRoundStart b Check = return b
evalRoundStart b (Bet m) = return $ (updateBotState (Raise m) b) -- Raise also take inv - WRONG

normaliseCalls :: [CompleteBot] -> [CompleteBot]
normaliseCalls bs = map normalise bs 
	where 
		normalise b@(p,s) = (p&currentCall .~ call, s&callNeeded .~ (call - (invested b))) 
		bots = filter (not . hasFolded) bs
		call = maximum $ map currentcall bs

 	
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

oneOrNonePlaying :: [CompleteBot] -> Bool
oneOrNonePlaying bs = 2 > (length $ filter (\x -> not x) $ map notPlaying bs)

everyoneAllInOrFolded :: [CompleteBot] -> Bool
everyoneAllInOrFolded bs = everyoneAllIn || everyoneFolded    
	where 
		everyoneAllIn = all isAllIn notFoldedBots
		everyoneFolded = null notFoldedBots
		notFoldedBots = filter (not . hasFolded) bs

finishedBetting :: [CompleteBot] -> Bool
finishedBetting bs = (everyoneAllInOrFolded bs) || everyoneBetTheSame   
	where 
		maxBet = maximum $ map invested playingBots 
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

testHand :: Hand
testHand = [(Club,Six),(Club,Two), (Club,Six),(Club,Four),(Club,Two)]

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

filterBotInList :: CompleteBot -> [CompleteBot] -> CompleteBot
filterBotInList (p,s) list = head $ filter (\(p1,_) -> p1^.name == p^.name ) list

collectBots :: [CompleteBot] -> [Pot2] -> [CompleteBot]
collectBots foldedBots pots = foldl (\list (_, bot@(p,s)) -> if isBotInList bot list then list else (p,s&investedInPot .~ 0) : list) foldedBots $ concat pots
   
dummyBots = [(folderBot "x", (botState [])&investedInPot .~ 23), 
	(folderBot "xx", (botState [])&investedInPot .~ 11), 
	(folderBot "xxx", (botState [])&investedInPot .~ 222),
	(folderBot "xxxx", (botState [])&investedInPot .~ 11)] 
