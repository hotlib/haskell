
import Control.Monad.State
import Control.Monad.Reader

data MyType = MT { runMT :: ReaderT Integer (State Integer) Integer}

myState :: State Integer Integer
myState = state $ \x -> (x + 1, x + 1)

myReader :: ReaderT Integer (State Integer) Integer
myReader = ReaderT (\_ -> myState)

defaultT :: MyType
defaultT = MT myReader

comp :: MyType
comp = MT $ do
		content <- ask
		myReader
		myReader
		return (content)

main = do 
	print $ runState (runReaderT ( runMT comp ) 4) 11
