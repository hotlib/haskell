--Implements the Stable marriage problem. The full description of the problem is:

--Given n men and n women, where each person has ranked all members of the opposite sex with an unique number
--between 1 and n in order of preference, marry the men and women together such that there are no two people 
--of opposite sex who would both rather have each other than their current partners. If there are no
--such people, all the marriages are "stable".

{-# LANGUAGE ViewPatterns #-}

import System.Random
import Control.Monad
import Data.List
import Control.Applicative
import Data.Maybe
import qualified Data.Map as M

-- |ID of a person
type PersonId = Int
-- |Preference toward a person, higher preference means higher attraction  
type Preference = Int

-- | Map represents engaged couples 
-- If a man is engaged to a women then key = woman ID and value = man ID 
type Couples = M.Map PersonId PersonId 

-- |Represents a man or a woman
-- I Added an ID to each person, although not necessary, it's easier when I want to plug entities into the algorithm
newtype Person = Person (PersonId, [(Preference, PersonId)]) deriving (Show)

--randomPreferences :: PersonId  -> IO Person 
--randomPreferences id = sortedNums >>= \n -> return $ Person (id, n)
--	where
--		sortedNums = randomNums -- sorted because of debug purposes 
--		randomNums = replicateM possibleMates $ randomRIO (1, 100) :: IO [Int] 

-- |Returns a list of person with the given ID (preferably with length == 1)
idFilter :: PersonId -> [Person] -> [Person]
idFilter id = filter . idCond $ id
	where idCond id (Person (idP, _)) = id == idP

-- |Returns a Person with the given ID
personWithId :: PersonId -> [Person] -> Maybe Person
personWithId id (idFilter id -> []) = Nothing
personWithId id (idFilter id -> [p]) = Just p

-- |Searches the given list and returns the preference with the given PersonId
preferenceById :: [(Preference, PersonId)] -> PersonId -> Preference
preferenceById xs personId = fst . head $ filter (\(pr, id) -> id == personId) xs

-- |True if first ID's preference is lower than second ID's (based on the given preference list)
lower :: [(Preference, PersonId)] -> PersonId -> PersonId -> Bool
lower prefs id1 id2 = (preferenceById prefs id1) < (preferenceById prefs id2)
	
-- |Engage PersonId to Person
engage :: PersonId -> Person -> Couples -> Couples
engage idMan (Person (idWoman, womanPreference)) map = 
	M.insertWith chooseBetter idWoman idMan map -- always engage with the better person 
	where 
		chooseBetter idNewMan idAlreadyEngagedMan 
			= if lower womanPreference idNewMan idAlreadyEngagedMan then idAlreadyEngagedMan else idNewMan

-- |Returns a list of engaged men or women IDs
engaged :: ((PersonId, PersonId) -> PersonId) -> Couples -> [PersonId] 
engaged f m = map f $ M.toList m

-- |Returns a list of engaged men IDs
engagedMenIds :: Couples -> [PersonId] 
engagedMenIds m = engaged snd m 

-- |Returns a list of engaged women IDs
engagedWomenIds :: Couples -> [PersonId] 
engagedWomenIds m = engaged fst m 

-- |Join person from two lists
-- If not present person from the second list (argument) are added to the first list (argument) 
merge :: [Person] -> [Person] -> [Person]
merge new old = foldl (\ps1 ps2 -> addPerson ps1 ps2) new old

-- |Adds a person to a list if he is missing in the list
addPerson :: [Person] -> Person -> [Person]  
addPerson ps p 
	| contains p ps = ps  
	| otherwise = p:ps
	where contains (Person (id,_)) ps = isJust $ personWithId id ps 

-- |Creates a person without his highest preference
removeMaxPreference :: Person -> Person
removeMaxPreference (Person (id, prefs)) = (Person (id, delete (maximum prefs) prefs))

-- |Returns the Person with highest preference
chooseBestFit :: [(Preference, PersonId)] -> [Person] -> Person
chooseBestFit prfs ps = 
	fromJust $ personWithId (highestPreferenceId prfs) ps
	where 
		highestPreferenceId prfs = snd (maximum prfs) 

-- |Person from first list propose to their highest preference person (from the second list) 
proposeToBestFit :: [Person] -> [Person] -> Couples -> Couples
proposeToBestFit unEngagedMen women m = foldl engageBestFit m unEngagedMen 
	where 
		 engageBestFit m (Person (idMan, pref)) = engage idMan (chooseBestFit pref women) m 

-- |Matches men and women based on preferences, returns a map of resulting couples
-- Implements the Gale–Shapley algorithm for the stable marriage problem
match :: [Person] -> [Person] -> Couples -> ([Person], Couples)
match men women m 
	| (length . engagedMenIds $ m) == (length men) || (length . engagedWomenIds $ m) == (length women) = (men, m) -- TODO other condition? not efficient
	| otherwise = 
		let 
			proposingMen = unengaged men m			
			couples = proposeToBestFit proposingMen women m
		in
			match (updatePreferences proposingMen men) women couples
		where
			updatePreferences proposingMen men = merge (removeProposedPreference proposingMen) men
			removeProposedPreference a = map removeMaxPreference a 
			unengaged men map = filterEngaged men (engagedMenIds m)
			filterEngaged ps engaged = filter (\(Person (id, _)) -> not $ id `elem` engaged) ps

main :: IO ()
main = do
	let men = [(Person (0, [(1,0),(3,1),(2,2)])), (Person (1, [(2,0),(3,1),(1,2)])), (Person (2, [(1,0),(2,1),(3,2)]))]
	let women = [(Person (0, [(1,0),(2,1),(3,2)])), (Person (1, [(2,0),(3,1),(1,2)])), (Person (2, [(1,0),(2,1),(3,2)]))]
	print $ match men women M.empty
	--where
	--	getPrefs = mapM randomPreferences [0 .. (possibleMates - 1)]
