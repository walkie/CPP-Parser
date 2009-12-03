
module Merge where

import Choice

type Degree = Int
type Index  = Int

-- Description of a new, merged dimension.
--  - name of the new dimension
--  - list of existing boolean dimensions to merge
--  - list of new tag names
data Merge t = Merge Name [Name] [t]

-- Description of how to merge a particular dimension.
--  - name of new dimension
--  - degree of new dimension
--  - index of this dimension in the new dimension
type MergeInfo = (Name, Degree, Index)

-- Mapping from existing dimension names to MergeInfos.
type InfoMap = [(Name, MergeInfo)]

-- Build InfoMap.
infoMap :: [Merge t] -> InfoMap
infoMap [] = []
infoMap (Merge n bs ts : ms) = [(b,(n,d,i)) | (b,i) <- zip bs [0..]] ++ infoMap ms
  where d = length ts

-- Generate the new dimension declarations.
decls :: [Merge t] -> Expr t a -> Expr t a
decls []                  = id
decls (Merge n _ ts : ms) = Dim (n := ts) . decls ms

-- Perform the merge.
dimMerge :: [Merge String] -> Expr Bool a -> Expr String a
dimMerge ms e = decls ms (m e)
  where info = infoMap ms
        merged = concatMap (\(Merge _ ns _) -> ns) ms
        m :: Expr Bool a -> Expr String a
        m (Var n)           = Var n
        m (a :< es)         = a :< map m es
        m (Let (v := e) f)  = Let (v := m e) (m f)
        m (Dim (n := ts) e) | elem n merged = m e
                            | otherwise     = Dim (n := ["defined", "undefined"]) (m e)
        m (b :? [t,f])      = case lookup b info of
          Just (n,d,i) -> Let ("f" := m f) $ -- TODO: this could clobber an existing variable!
                          n :? (replicate i (Var "f") ++ (m t) : replicate (d-i-1) (Var "f"))
          Nothing      -> b :? [m t, m f]

e = Dim ("A" := [True, False]) $ 
    Dim ("B" := [True, False]) $ 
    1 :< ["A" :? [leaf 2, leaf 3], 
          "B" :? [leaf 4, leaf 5], 
          "A" :? [leaf 6, "B" :? [leaf 7, leaf 8]],
          "A" :? ["B" :? [leaf 9, leaf 10], leaf 11],
          "B" :? [leaf 12, "A" :? [leaf 13, leaf 14]]
         ] 

e' = dimMerge [Merge "AB" ["A","B"] ["A","B","X"]] e
