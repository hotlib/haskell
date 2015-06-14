
-- out :: Fix f -> f (Fix f)
-- *Main> :t in
--
-- <interactive>:1:1: parse error on input `in'
-- *Main> :t In
-- In :: f (Fix f) -> Fix f
--


data Tree' v = Op1' v (Tree' v) (Tree' v) 
	    | Op2' v (Tree' v) (Tree' v) 
            | Leaf' deriving Show

-- TreeF value carrier
-- carrier type is arbitrary
data TreeF v c = Op1 v c c
               | Op2 v c c 
	       | Leaf deriving Show

newtype Fix f = In (f (Fix f)) 

out :: Fix f -> f (Fix f)
out (In x) = x

exampleTree' :: Tree' Int
exampleTree' = Op1' 5 (Op2' 5 (Op1' 6 Leaf' Leaf') Leaf') (Op2' 7 Leaf' Leaf')

type Tree v = Fix (TreeF v) 

instance Show (Fix f) where
 show _ = "a f-tree"

instance Functor (TreeF v) where
    fmap f Leaf = Leaf
    fmap f (Op1 v l r) = Op1 v (f l) (f r)
    fmap f (Op2 v l r) = Op2 v (f l) (f r)


-- algebra describes how to map a functor where the recursive
-- positions have already been evaluated to a result

intAlg :: TreeF Int Int -> Int
intAlg Leaf = 1
intAlg (Op1 v l r) = v * (l + r)
intAlg (Op2 v l r) = v * (l - r)

exampleTree :: Tree Int
exampleTree = In $ 
     Op1 5 
       (In $ Op2 5 
         (In $ Op1 6 
	   (In Leaf) (In Leaf))
         (In Leaf)) 
       (In $ Op2 7 (In Leaf) (In Leaf))

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . out

exampleTreeF = Op1 4 (Op2 3 (Op1 4 Leaf Leaf) Leaf) Leaf 

main :: IO ()
main = do 
	print $ cata intAlg exampleTree 
        print exampleTree
	print "test"

