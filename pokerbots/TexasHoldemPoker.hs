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
round_ t = smallBlindBet t >>= bigBlindBet >>= normalRound >>= betRound 

betRound :: TexasHoldemPoker -> GamePlay TexasHoldemPoker
betRound t = do 
	newT <- betRound  	
	if not . everyoneAllInOrFolded $ newT^.bots then
		normalRound newT
	else
		return newT	
	 where
	 	betRound = iterateUntilM (\thp -> hasBet oldBots (updatedBot thp) || (everyoneAllInOrFolded $ thp^.bots)) playBetsOnOneBot t
	 	oldBots = t^.bots
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
	putStrLn $ snd x
	print $ fst x
	
	-- x <- playBot folderBot (botState [])
	-- print x
	-- print $ splitPots3 [] dummyBots
	-- print "ok"