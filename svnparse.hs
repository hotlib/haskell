import Text.Parsec.String
import Text.Parsec.Prim
import Text.Parsec
import Text.ParserCombinators.Parsec hiding (try)
import Control.Applicative hiding (many, (<|>))
import Control.Monad

data SvnEntry = SvnEntry { getMessage :: String, getId::Int, getName :: String, getDate :: String, getSvnActions::[SvnAction] } deriving( Show ) 
data SvnAction = SvnAction { getAction :: Char, getPath :: String} deriving( Show ) 



svnChanges :: Parser SvnAction
svnChanges =  liftA2 SvnAction (spaces >> anyChar) (spaces >> (many $ noneOf "\n\r"))
				

svnEntry ::  Parser SvnEntry
svnEntry = SvnEntry 
	<$> (manyTill anyChar (try brakeline)) 
	<*> (many (oneOf "-\n\rr") >> many1 digit >>= (\n -> return (read n))) 
	<*> (separator >> many1 alphaNum) 
	<*> (separator >> manyTill anyChar (try separator)) 		
 	<*> (skipSomePart >> manyTill svnChanges ( choice [try newnewline, try myEof]))   
 	where 
 		separator = string " | "
 		newnewline = string "\r\n\r\n"
 		skipSomePart = manyTill anyChar (try (string "  "))
 		myEof = (eof >>= \xx -> return "eof") :: Parser String
 		brakeline = string "----------"

main :: IO ()
main = do
	 	
	 	s <- readFile "svn_test.log"
	 	let a = parse (count 3 svnEntry) "test" s
	 	let b = parse (many svnEntry) "test" s
	 	print b
		-- print $ length <$> b
	 	