
module Semantics where

import Data.List (intersperse, nub)

import Choice hiding (dom,sem,variants)

type Dim t = (Name, [t])

data Dom t = Cat [Dom t] -- sequence
           | Bar [Dom t] -- "or"
           | D (Dim t)   -- a single dimension
           | Nil         -- the empty domain
           deriving Eq

-- The semantic domain.
dom :: Eq t => Expr t a -> Dom t
dom (_ :< es)         = cross (map dom es)
dom (Dim (n := ts) e) = cross [D (n,ts), dom e]
dom (_ :? es)         = union (map dom es)
dom (Let (v := e) e') = cross [dom e, dom e']
dom (Var v)           = Nil

{- This is wrong, I'm pretty sure.  Dims in lets should have syntactic scoping.
dom' :: Eq t => Map (Dom t) -> Expr t a -> Dom t
dom' m (_ :< es)         = cross (map (dom' m) es)
dom' m (Dim (n := ts) e) = cross [D (n,ts), dom' m e]
dom' m (_ :? es)         = union (map (dom' m) es)
dom' m (Let (v := e) e') = dom' ((v,(dom' m e)):m) e'
dom' m (Var v)           = maybe Nil id (lookup v m)
-}

-- Turn a domain into a list of selections.
flatten :: Dom t -> [[QTag t]]
flatten Nil        = []
flatten (D (n,ts)) = [[(n,t)] | t <- ts]
flatten (Cat ds)   = map concat (perms (map flatten ds))
flatten (Bar ds)   = concatMap flatten ds

-- All sequences from a list of lists. i.e.:
-- > perms [[1,2],[3,4],[5,6]]
-- [[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]]
perms []       = []
perms [qs]     = [[q] | q <- qs]
perms (qs:qss) = [r:rs | r <- qs, rs <- perms qss]

-- The semantic function.
sem :: (Show t,Eq t) => Expr t a -> Variety t a
sem e = (map sel . nub . flatten . dom) e
  where sel qs = (qs, foldr (uncurry select) e qs)

-- Pretty print the semantics of an expression.
printSem :: (Eq t, ShowNesting a, ShowNesting t) => Expr t a -> IO ()
printSem = putStr . showVar . sem
  where showVar    = unlines . map row
        row (qs,e) = qns qs ++ ": " ++ show e
        qns qs     = "[" ++ concat (intersperse "," (map qn qs)) ++ "]"
        qn (d,t)   = showS d ++ "." ++ showS t

-- Sequential combination.
cross :: Eq t => [Dom t] -> Dom t
cross = foldr c Nil
  where c Nil      d        = d
        c d        Nil      = d
        c (Cat as) (Cat bs) = Cat (as ++ bs)
        c (Cat as) b        = Cat (as ++ [b])
        c a        (Cat bs) = Cat (a : bs)
        c a        b        = Cat [a,b]

-- Parallel/bar combination.
union :: Eq t => [Dom t] -> Dom t
union = foldr u Nil
  where u Nil      d        = d
        u d        Nil      = d
        u (Bar as) (Bar bs) = bar (as ++ bs)
        u (Bar as) b        = bar (as ++ [b])
        u a        (Bar bs) = bar (a : bs)
        u a        b        = bar [a,b]
        bar = Bar . nub

[n1,n2,n3,n4] = map leaf ["1","2","3","4"]

-- dim A<a,b> in
-- A<dim B<c,d> in B<1,2>, dim B<d,c> B<2,1>>
redA2 = Dim ("A" := ["a","b"])
     $ "A" :? [Dim ("B" := ["c","d"]) $ "B" :? [n1,n2],
               Dim ("B" := ["d","c"]) $ "B" :? [n2,n1]]

-- dim A<a,b> in
-- A<dim B<c,d> in B<1,2>, dim C<d,c> C<2,1>>
redA2' = Dim ("A" := ["a","b"])
     $ "A" :? [Dim ("B" := ["c","d"]) $ "B" :? [n1,n2],
               Dim ("C" := ["d","c"]) $ "C" :? [n2,n1]]

-- dim A<a,b> in 
-- A<dim B<c,d> in B<1,2>, dim B<c,d> in B<3,4>>
e = Dim ("A" := ["a","b"])
     $ "A" :? [Dim ("B" := ["c","d"]) $ "B" :? [n1,n2],
               Dim ("B" := ["c","d"]) $ "B" :? [n3,n4]]

-- dim A<a,b> in 
-- A<dim B<c,d> in B<1,2>, dim B<e,f> in B<3,4>>
e' = Dim ("A" := ["a","b"])
     $ "A" :? [Dim ("B" := ["c","d"]) $ "B" :? [n1,n2],
               Dim ("B" := ["e","f"]) $ "B" :? [n3,n4]]

-- p { dim Var<x,y> in let v := Var<x,y> in add v = v + v,
--     dim Var<x,y> in let v := Var<x,y> in sub v = v - v }
p = "p" :< [Dim ("Var" := ["x","y"]) $ 
             Let ("v" := ("Var" :? [leaf "x", leaf "y"]))
               (parse "add @v = @v + @v"),
            Dim ("Var" := ["x","y"]) $ 
             Let ("v" := ("Var" :? [leaf "x", leaf "y"])) 
               (parse "sub @v = @v + @v")]

instance Show (Dom t) where
  show (D (n,_)) = n
  show (Cat ds)  = show ds
  show (Bar ds)  = concat (intersperse "|" (map show ds))
  show Nil       = "<>"
