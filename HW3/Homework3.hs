import Data.List (group, intersect, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as S

type Grammar = [(String, [[String]])]

type Sets = [(String, [String])]

myMap :: Integer -> Map Integer [Integer]
myMap n = Map.fromList (map makePair [1 .. n])
  where
    makePair x = (x, [x])

--Define a representation for context-free grammars.
input1 :: Grammar
input1 = [("E", [["T", "E'"]]), ("E'", [["+", "T", "E'"], ["eps"]]), ("T", [["F", "T'"]]), ("T'", [["*", "F", "T'"], ["eps"]]), ("F", [["(", "E", ")"], ["id"]])]

input2 :: Grammar
input2 = [("E", [["T'", "E'"]]), ("E'", [["+", "T", "E'"], ["eps"]]), ("T", [["F", "T'"]]), ("T'", [["*", "F", "T'"], ["eps"]]), ("F", [["(", "E", ")"], ["id"]])]

input3 :: Grammar
input3 = [("A", [["b", "C"], ["B", "d"]]), ("B", [["C", "C"], ["b", "A"]]), ("C", [["c", "C"], ["eps"]])]

pp :: Sets -> IO ()
pp x = sequence_ (map print x)

--Create a function computing non-terminals, which can be rewritten to 'epsilon' (empty set).
getEmptyNonTerms :: Grammar -> [String]
getEmptyNonTerms input = getEmptyNonTerms' input (getKeys input) [] (filter (/= []) [if lstContainsWord "eps" inputR then inputL else [] | (inputL, inputR) <- input])

getEmptyNonTerms' :: Grammar -> [String] -> [String] -> [String] -> [String]
getEmptyNonTerms' input nonTerms before after
  | after == [] = before
  | otherwise = getEmptyNonTerms' input nonTerms (before ++ after) (filter (/= []) [if elem True ([compareLists (intersect (intersect nonTerms eachR) after) eachR | eachR <- inputR]) then inputL else [] | (inputL, inputR) <- filteredInput])
  where
    filteredInput = filter (\(a, _) -> (elem a before) == False) input

lstContainsWord :: String -> [[String]] -> Bool
lstContainsWord word list = filter (== True) ([elem word l | l <- list]) /= []

compareLists :: [String] -> [String] -> Bool
compareLists xs ys = S.fromList xs == S.fromList ys

getKeys :: Grammar -> [String]
getKeys input = ([i | (i, _) <- input])

--Create a function computing the first set for giver sequence of symbols (terminals and non-terminals).
getFirstSets :: Grammar -> Sets
getFirstSets input = [(xl, getFirstSets' xl input (getKeys input)) | (xl, xr) <- input]

getFirstSets' :: String -> Grammar -> [String] -> [String]
getFirstSets' key input keys = rmdups (concat output)
  where
    output = [if (elem (values !! 0) keys) then (getFirstSets' (values !! 0) input keys) else [values !! 0] | values <- rightSide]
    (_, rightSide) = (filter ((== key) . fst) input) !! 0

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

--Create a function computing the follow set for the given non-terminal.
--Bohužel jsem nebyl schopen tento problém převést do funkcionálního programování
getFollowSets :: Grammar -> Sets
getFollowSets input = output2
  where
    output2 = concat([[(inpL,(filter (/=[])[if(elem value keys) then value else "" |value<-values]))|values<-inpR]|(inpL,inpR)<-input])
    output = addToSets firstKey "$" [(inpL, []) | (inpL, _) <- input]
    (firstKey, _) = input !! 0
    keys = getKeys input

addToSets :: String -> String -> Sets -> Sets
addToSets key value input = [if key == inpL then (inpL, inpR ++ [value]) else (inpL, inpR) | (inpL, inpR) <- input]

getFollows' :: Grammar -> [(String, String)]
getFollows' input = concat ([getFollows keys x|x<-input])
    where
        keys = getKeys input

getFollows :: [String] -> (String,[[String]]) -> [(String, String)]
getFollows keys (inpL,inpR) = concat ([filter (\(_, a) -> a /="") ([(value,if (elem value keys) then inpL else "")|value<-values ])|values<-inpR])

goThruList :: [String] -> [String] -> [[String]]
goThruList keys input = [[inputAdd!!(i-1),inputAdd!!i,inputAdd!!(i+1)] |i<-[1..(length inputAdd)-2]]
    where
        inputAdd = input ++ [""]