
module Algorithm where

import Trie

import Data.Function
import Data.List (delete,group,minimumBy,permutations,sort,sortBy)

----------------------
-- Greedy Algorithm --
----------------------

type Freq a = [(a,Int)]

-- Build a frequency map of the characters used in words.  We really want to
-- know how many words a character appears in, but since we can assume that
-- each character (feature) appears only once in each word (feature selection),
-- this works just fine.
freq :: Ord a => [Word a] -> Freq a
freq = map (\l -> (head l, length l)) . group . sort . concat

next :: Ord a => [Word a] -> a
next = fst . head . sortBy (compare `on` negate . snd) . freq

partition :: Eq a => a -> [Word a] -> ([Word a], [Word a])
partition a ws = (has ws, hasnt ws)
  where has = map (delete a) . filter (elem a)
        hasnt = filter (not . elem a)

step :: Ord a => [Word a] -> [(a,Trie a)]
step [] = []
step ws = (a, greedy has) : step hasnt
  where a = next ws
        (has,hasnt) = partition a ws

greedy :: Ord a => [Word a] -> Trie a
greedy ws = Trie (any null ws) (step (filter (not . null) ws))

---------------------------
-- Brute-Force Algorithm --
---------------------------

cost :: Trie a -> Int
cost (Trie _ m) = length m + sum (map (cost . snd) m)

cross :: [[a]] -> [[a]]
cross (xs:xss) = [y:ys | y <- xs, ys <- cross xss]
cross [] = [[]]

perms :: [Word a] -> [[Word a]]
perms = concat . map permutations . cross . map permutations

brute :: Ord a => [Word a] -> Trie a
brute = minimumBy (compare `on` cost) . map build . perms
