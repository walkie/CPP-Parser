
module Transform where

import Data.List

import Choice

varsets :: Expr t a -> [[Name]]
varsets = indySets . expr []
  where expr m (Let (v := e) f) = expr ((v,expr m e):m) f
        expr m (Var v)   = maybe [] id (lookup v m)
        expr m (a :< []) = []
        expr m (a :< es) = exprs m es
        expr m (Dim _ e) = expr m e
        expr m (d :? es) = indySets (add d (exprs m es))
        exprs m = indySets . concatMap (expr m)

add :: a -> [[a]] -> [[a]]
add a [] = [[a]]
add a ss = map (a:) ss

indySets :: Eq a => [[a]] -> [[a]]
indySets []     = []
indySets (s:ss) = 
    case partition (null . intersect s) ss of
      (ns,[]) -> nub s : indySets ns
      (ns,ys) -> indySets (foldr union s ys : ns)
