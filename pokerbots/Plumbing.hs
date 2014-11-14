
module Plumbing where

import Defs
import Evaluation
import Control.Lens hiding (Fold, folded)
import Control.Monad.Trans.Cont
import Data.Maybe 

notPlaying :: CompleteBot -> Bool	
notPlaying b = hasFolded b || isAllIn b

isAllIn :: CompleteBot -> Bool
isAllIn b = (fst b)^.botStatus == AllIn

hasFolded :: CompleteBot -> Bool
hasFolded b = (fst b)^.botStatus == Folded

setFolded :: CompleteBot -> CompleteBot
setFolded b = ((fst b)&botStatus .~ Folded, snd b)

setAllIn :: CompleteBot -> CompleteBot
setAllIn b = ((fst b)&botStatus .~ AllIn, snd b)

invested :: CompleteBot -> Money
invested = ((^.investedInPot) . snd) 

callneeded :: CompleteBot -> Money
callneeded = ((^.callNeeded) . snd)

moneyleft :: CompleteBot -> Money
moneyleft = ((^.moneyLeft) . snd)

currentcall :: CompleteBot -> Money
currentcall = ((^.currentCall) . fst)

botState :: [Card] -> BotState
botState c = BotState {_hole = c, _moneyLeft = 100, _investedInPot = 0, _callNeeded = 0, _potTotal = [], _communityCards = []}	

transform :: Hand -> HandEvaluation
transform h = fromJust $ runCont (evaluateHand h) id

simpleBot :: CompleteBot -> BotSimple
simpleBot b = BotSimple { _money = (moneyleft b), _botName = (botName_ b), _status = (botState_ b) }
	where
		botState_ = ((^.botStatus) . fst)
		botName_ = ((^.name) . fst)

bblind :: Money
bblind = 10

sblind :: Money
sblind = 5

pokerBot :: String -> PokerAction RoundStartAction -> PokerAction PlayAction -> PokerBot
pokerBot n r p = PokerBot { _name = n, _startAction = r, _playAction = p, _botStatus = Playing, _currentCall = 0}

folderBot :: String -> PokerBot
folderBot name = pokerBot name (return Fold_) (return Fold)

callBot :: String -> PokerBot
callBot name = pokerBot name (return (Bet 20)) (return Call)

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