{-# LANGUAGE PatternGuards #-}

module RCS where

import Data.List  (elemIndex)
--import Data.Maybe (isJust,fromJust,fromMaybe)

import Choice hiding (Map,select,selectIx)

-----------------------
-- Vanilla Selection --
-----------------------

type QIx         = (Name,Int)
type Context t a = Expr t a -> Expr t a

select :: Eq t => QTag t -> Expr t a -> Maybe (Expr t a)
select q e | Just (s,c,e') <- match q e = Just (c (selectIx s e'))
           | otherwise                  = Nothing

-- This doesn't capture the context correctly yet!
match :: Eq t => QTag t -> Expr t a -> Maybe (QIx, Context t a, Expr t a)
match _ v@(Var _)         = Nothing
match q (_ :< es)         = firstJust (map (match q) es)
match q (Let (_ := e) e') = firstJust (map (match q) [e,e'])
match q (_ :? as)         = firstJust (map (match q) as)
match q@(d,t) (Dim (d' := ts) e) 
  | d == d', Just i <- elemIndex t ts = Just ((d,i), id, e)
  | otherwise                         = match q e

-- scrap your boilerplate would be good here...
selectIx :: QIx -> Expr t a -> Expr t a
selectIx _ v@(Var _)           = v
selectIx s (a :< es)           = a :< map (selectIx s) es
selectIx s (Let (v := e) e')   = Let (v := selectIx s e) (selectIx s e')
selectIx s x@(Dim (d := ts) e) | fst s == d = x
                               | otherwise  = Dim (d := ts) (selectIx s e)
selectIx s@(d,i) (d' :? as)    | d == d'    = selectIx s (as !! i)
                               | otherwise  = d' :? map (selectIx s) as

---------------
-- Tag Trees --
---------------

type Map k v   = [(k,v)]
type TagTree t = Map (QTag t) (QTag t)

selectTT :: Eq t => TagTree t -> QTag t -> Expr t a -> Maybe (Expr t a)
selectTT tt q e = foldlMaybe (flip select) e (path tt q)

path :: Eq t => TagTree t -> QTag t -> [QTag t]
path tt = reverse . initJusts . iterMaybe (flip lookup tt)

-----------------------
-- Utility functions --
-----------------------

-- firstJust = listToMaybe . dropWhile isNothing
firstJust :: [Maybe a] -> Maybe a
firstJust []           = Nothing
firstJust (Nothing:ms) = firstJust ms
firstJust (Just a :ms) = Just a

-- initJusts = map fromJust . takeWhile isJust
initJusts :: [Maybe a] -> [a]
initJusts []           = []
initJusts (Nothing:ms) = []
initJusts (Just a :ms) = a : initJusts ms

iterMaybe :: (a -> Maybe a) -> a -> [Maybe a]
iterMaybe f = iterate (maybe Nothing f) . Just

foldlMaybe :: (a -> b -> Maybe a) -> a -> [b] -> Maybe a
foldlMaybe f = foldl (\m b -> maybe Nothing (\a -> f a b) m) . Just

--------------
-- Examples --
--------------

p :: Int -> Name
p n = "P" ++ show n

patch :: Int -> QTag Bool
patch n = (p n, True)

patchDim :: Int -> Expr Bool a -> Expr Bool a
patchDim n = Dim ((p n) := [True,False])

-- 1
-- `-> 2
--     `-> 3
--     `-> 4
patchTree :: TagTree Bool
patchTree = [(patch 3, patch 2), (patch 2, patch 1), (patch 4, patch 2)]

ex0 :: Expr Bool String
ex0 = "" :< [leaf "aa",
             leaf "bb",
             leaf "cc"]

ex1 = patchDim 1
    $ "" :< [leaf "aa",
             p 1 :? [leaf "BB", leaf "bb"],
             leaf "cc"]

-- Would be easier to just put new dimension decl at the top, but my
-- implementation doesn't yet handle contexts correctly, so decls must
-- be added in a particular order for now...
ex2 = patchDim 1
    $ patchDim 2
    $ "" :< [leaf "aa",
             p 1 :? [leaf "BB", leaf "bb"],
             p 2 :? [leaf "CC", leaf "cc"]]

ex3 = patchDim 1
    $ patchDim 2
    $ patchDim 3
    $ "" :< [leaf "aa",
             p 1 :? [p 3 :? [leaf "$$", leaf "BB"], leaf "bb"],
             p 2 :? [leaf "CC", leaf "cc"]]

ex4 = patchDim 1
    $ patchDim 2
    $ patchDim 3
    $ patchDim 4
    $ "" :< [leaf "aa",
             p 1 :? [p 3 :? [leaf "$$", 
                     p 4 :? [leaf "££", leaf "BB"]], leaf "bb"],
             p 2 :? [leaf "CC", leaf "cc"]]
