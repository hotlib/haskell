{-# LANGUAGE FlexibleContexts #-}

import Text.Parsec.Pos
import Text.Parsec
import Data.List 
import Safe

data Token = LeftBracket
           | RightBracket 
           | Plus
	   | Minus
           | LeftShift
	   | RightShift
           | Print
	   | Input
	   deriving (Show, Eq)

data Tree = Op Token Tree
          | Loop Tree Tree
	  | End
          deriving (Show, Eq) 

main :: IO ()
main = do
        let (Right x) = parse (many1 tokenizer) "" "+[-[+]-]++"
	print $ buildTree x

buildTree :: [Token] -> Tree
buildTree [] = End
buildTree ts@(LeftBracket : _) = let (firstPart, restPart) = mySplit 
					 in Loop (buildTree $ tailSafe firstPart) (buildTree $ tailSafe restPart)   
    where
      mySplit = splitAt (snd bracketTuple) ts
      bracketTuple = head . filter (\x -> fst x == 0) $ bracketIndices ts      

buildTree (x : rest) = Op x $ buildTree rest  

matchIndices :: [Int] -> [Int] -> [(Int, Int)]
matchIndices ls rs = foldl match [] rs 
    where
      match acc r = (leftBracketIndex acc r, r) : acc
      leftBracketIndex acc r = index r (map fst acc) 
      index r used = maximum $ filter (\x -> x < r && x `notElem` used) ls  
  	      

bracketIndices :: [Token] -> [(Int,Int)]
bracketIndices ts = matchIndices leftIndices rightIndices 
    where 
      getIndices what = findIndices (== what) ts
      leftIndices = getIndices LeftBracket
      rightIndices = getIndices RightBracket


recognise :: Char -> Maybe Token
recognise c 
       | c == '+' = Just Plus
       | c == '-' = Just Minus
       | c == '<' = Just LeftShift
       | c == '>' = Just RightShift
       | c == '.' = Just Print
       | c == ',' = Just Input
       | c == '[' = Just LeftBracket
       | c == ']' = Just RightBracket
       | otherwise = error "unsupported character"

satisfy2 :: (Stream s m Char) => (Char -> Maybe Token) -> ParsecT s u m Token
satisfy2 f = tokenPrim (\c -> show [c])
              (\pos c _cs -> updatePosChar pos c)
	      f

tokenizer :: (Stream s m Char) => ParsecT s u m Token
tokenizer = satisfy2 recognise

-- eval :: String -> Token -> String
-- eval s End = s ++ " the end " 
-- eval s (Plus x) = eval (s ++ " a plus ") x
