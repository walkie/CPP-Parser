
module Trie where

import Data.List (intersperse)
import Data.Tree (Tree(..),drawTree)

-----------
-- Tries --
-----------

type Word a = [a]
data Trie a = Trie Bool [(a,Trie a)]

-- An empty trie.
nil :: Trie a
nil = Trie False []

-- Add a word to the trie.
add :: Ord a => Word a -> Trie a -> Trie a
add []     (Trie _ m) = Trie True m
add (a:as) (Trie b m) = Trie b (update nil a (add as) m)

-- Args: default value, key of value to update, update function, map
update :: Eq k => v -> k -> (v -> v) -> [(k,v)] -> [(k,v)]
update d a f [] = [(a,f d)]
update d a f ((k,v):m) | a == k    = (k, f v) : m
                       | otherwise = (k, v)   : update d a f m

-- Add a list of words to the trie.
addAll :: Ord a => Trie a -> [Word a] -> Trie a
addAll = foldl (flip add)

-- Build a trie from a list of words.
build :: Ord a => [Word a] -> Trie a
build = addAll nil


---------------------
-- Pretty Printing --
---------------------

-- Draw as a tree. TODO: distinguish True and False nodes.

-- Turn a trie into a list of trees.
toTrees :: Trie a -> [Tree a]
toTrees (Trie _ m) = [Node a (toTrees t) | (a,t) <- m]

toStringTree :: Show' a => Trie a -> Tree String
toStringTree = Node "" . map (fmap show') . toTrees

-- Filter out drawTree's gratuitous vertical space.
condense = unlines . filter (not . all empty) . lines
  where empty ' ' = True
        empty '|' = True
        empty _   = False

asTree :: Show' a => Trie a -> String
asTree = condense . drawTree . toStringTree


-- Draw as an equation. TODO: distinguish True and False nodes.

branch :: Show' a => (a, Trie a) -> String
branch (a,t) = show' a ++ asEq' t

branches :: Show' a => [(a, Trie a)] -> String
branches = concat . intersperse "+" . map branch

asEq' :: Show' a => Trie a -> String
asEq' (Trie _ [])  = ""
asEq' (Trie _ [b]) = branch b
asEq' (Trie _ m)   = "(" ++ branches m ++ ")"

asEq :: Show' a => Trie a -> String
asEq (Trie _ m) = branches m


-- Show instances

class Show' a where
  show' :: a -> String
instance Show' Char where
  show' c = [c]
instance Show' Int where
  show' = show

instance Show' a => Show (Trie a) where
  show = asEq
