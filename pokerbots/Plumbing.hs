
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