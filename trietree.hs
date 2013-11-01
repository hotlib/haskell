
import qualified Data.Map as Map (Map, insert, empty, lookup)
import Prelude hiding (lookup)

data TrieNode k v = TrieNode { value :: Maybe v, children :: Map.Map k (TrieNode k v) } deriving (Show)

empty :: (Ord k) => TrieNode k v
empty  = trieNode Nothing Map.empty 

trieNode ::  Maybe v -> Map.Map k (TrieNode k v) -> TrieNode k v
trieNode value children = TrieNode { value = value, children = children }
	
lookUpTrieNode :: (Ord k) => k -> TrieNode k v -> TrieNode k v
lookUpTrieNode k n = case Map.lookup k (children n) of
	Nothing -> empty
	Just node -> node

insert :: (Ord k) => [k] -> v -> TrieNode k v -> TrieNode k v
insert [] v node = node { value = Just v } 
insert (k:ks) v node = trieNode (value node) map
	where 
		map = Map.insert k n c
		n = insert ks v $ lookUpTrieNode k node
		c = children node

main = do
	let e1 = insert [11] 9999 $ empty
	print $ (e1 :: TrieNode Int Int) 
	let e2 = insert [11, 22] 222 e1
	print $ (e2 :: TrieNode Int Int) 
	let e3 = insert [11, 33, 44] 333 e2
	print $ (e3 :: TrieNode Int Int)
