module TexasHoldemPoker where

import Defs
import Plumbing
import Evaluation
import Pokerbot
import Control.Monad.Loops
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import Data.List.Split
import Control.Lens hiding (Fold, folded)
-- import qualified Data.Map.Strict as Map

smallBlindBet :: TexasHoldemPoker -> GamePlay TexasHoldemPoker
smallBlindBet t = return $ t&bots %~ \(b:bs) -> bs ++ [(fst b, invest sblind $ snd b)]

bigBlindBet :: TexasHoldemPoker -> GamePlay TexasHoldemPoker
bigBlindBet t = return $ t&bots %~ \(b:bs) -> bs ++ [(fst b, (invest bblind $ snd b))]

instance PokerGame TexasHoldemPoker where
	initGame = initGame_
	round = round_
	playGame = playGame_ 

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

round_ :: TexasHoldemPoker -> GamePlay TexasHoldemPoker
round_ t = smallBlindBet t >>= bigBlindBet >>= normalRound >>= step >>= step >>= step >>= evalWinners
	where 
		step = \t -> addCommunityCard t >>= bettingRound 
		

--evalBotHands :: [CompleteBot] -> Map.Map String HandEvaluation
--evalBotHands bs = foldl addtoMap Map.empty bs
--	where 
--		addtoMap m (p,s) = Map.insert (p^.name) (evalHand $ cards s) m
--		cards s = (s^.communityCards) ++ (s^.hole) 

evalWinners :: TexasHoldemPoker -> GamePlay TexasHoldemPoker
evalWinners t = do
	liftIO $ print "XXXXXXXXXXXXXXXXX"
	liftIO $ print $ winnerlist
	return $ t&bots .~ (addMoney winnerlist)
	where
		winnerlist = map (winnerList $ t^.bots) aPot
		addMoney = winnersAddMoney [] 
		aPot = _potTotal . snd . head $ t^.bots
	

pickWinner :: TexasHoldemPoker -> IO ()
pickWinner t = 
	print $ map (winnerList $ t^.bots) aPot 
	where
		aPot = _potTotal . snd . head $ t^.bots
	
bettingRound :: TexasHoldemPoker -> GamePlay TexasHoldemPoker
bettingRound t = do 
	newT <- betRound  	
	if not . everyoneAllInOrFolded $ newT^.bots then
		normalRound newT
	else
		return newT	
	 where
	 	betRound = iterateUntilM (\thp -> hasBet (t^.bots) (updatedBot thp) || (oneOrNonePlaying $ thp^.bots)) playBetsOnOneBot t
	 	updatedBot thp = last $ thp^.bots
	 	hasBet bz b = (invested b) > (invested $ filterBotInList b bz)

playBetsOnOneBot :: TexasHoldemPoker -> GamePlay TexasHoldemPoker
playBetsOnOneBot t =  
	liftIO playBot >>= return 
	where
		playBot = return firstBot >>= playRoundStartBot2 >>= (evalRoundStart firstBot) >>= updateBot
		updateBot bot = return (t&bots .~ ((tail $ t^.bots) ++ [bot]))
		firstBot = head $ t^.bots

normalRound :: TexasHoldemPoker -> GamePlay TexasHoldemPoker
normalRound t = iterateUntilM (\thp -> finishedBetting $ thp^.bots) normalRound2 t


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

addCommunityCard :: TexasHoldemPoker -> GamePlay TexasHoldemPoker
addCommunityCard t = return $ t & updateDeckAndBots
	where 
		updateDeckAndBots = (deck %~ (drop 1)) . (bots %~ updatedBots)
		updatedBots = map $ \(p,s) -> (p, s&communityCards .~ (addedCard s))
		addedCard s = newCard : (s^.communityCards)
		newCard = head $ t^.deck


runGame :: (PokerGame g) => IO (g, String)
runGame = runWriterT $ initGame dummyBots2 >>= playGame  

defaultMain = do
	x <- runGame :: IO (TexasHoldemPoker, String)
	-- putStrLn $ snd x
	-- print $ fst x
	-- pickWinner $ fst x
	
	-- x <- playBot folderBot (botState [])
	-- print x
	-- print $ splitPots3 [] dummyBots
	print "ok"