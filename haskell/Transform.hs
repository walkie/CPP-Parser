{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}

module Transform where

import Data.List  (delete, nub)
import Data.Maybe (fromJust, isJust)

import Choice

--------------------------
-- Transformation Rules --
--------------------------

type Transform t a = Expr t a -> Maybe (Expr t a)

--
-- Choice commutation: left-to-right
-- 

csL :: Int -> Transform t a
csL i (x :< es) | (h, (d :? as):t) <- splitAt i es =
  Just (d :? [x :< (h ++ a:t) | a <- as])
csL _ _ = Nothing

cbdL :: Transform t a
cbdL (Let (v := (d :? as)) e) = Just (d :? [Let (v := a) e | a <- as])
cbdL _ = Nothing

cbuL :: Transform t a
cbuL (Let b (d :? as)) = Just (d :? [Let b a | a <- as])
cbuL _ = Nothing

cdL :: Transform t a
cdL (Dim b@(d' := _) (d :? as)) | d /= d' = Just (d :? [Dim b a | a <- as])
cdL _ = Nothing

ccsL :: Int -> Transform t a
ccsL i (d' :? as) | (h, (d :? bs):t) <- splitAt i as =
  Just (d :? [d' :? (h ++ b:t) | b <- bs])
ccsL _ _ = Nothing

ccmL :: Int -> Transform t a
ccmL i (d :? as) | (h, a:t) <- splitAt i as = 
  Just (d :? (h ++ (d :? (replicate (length as) a)):t))
ccmL _ _ = Nothing

--
-- Choice commutation: right-to-left
--

csR :: (Eq t, Eq a) => Int -> Transform t a
csR i (d :? ((x :< es):as)) | i < length es
                            , Just as' <- mergeMaybes (map ei as) =
    Just (x :< (h ++ (d :? (e : as')):t))
  where (h, e:t) = splitAt i es
        ei (y :< fs) | (g,f:s) <- splitAt i fs, (x,h,t) == (y,g,s) = Just f
        ei _ = Nothing
csR _ _ = Nothing

-- In the next two rules, all variable names must be identical.  This isn't
-- necessary for correctness, only for symmetry (i.e. cbdL . cbdR = id).
-- Regardless, it is implemented this way to be consistent with the paper.
cbdR :: (Eq t, Eq a) => Transform t a
cbdR (d :? (Let (v := a) e : ls)) | Just as <- mergeMaybes (map ai ls) =
    Just (Let (v := (d :? (a:as))) e)
  where ai (Let (w := b) f) | (v,e) == (w,f) = Just b
        ai _ = Nothing
cbdR _ = Nothing

cbuR :: (Eq t, Eq a) => Transform t a
cbuR (d :? (Let (v := e) a : ls)) | Just as <- mergeMaybes (map ai ls) =
    Just (Let (v := e) (d :? (a:as)))
  where ai (Let (w := f) b) | (v,e) == (w,f) = Just b
        ai _ = Nothing
cbuR _ = Nothing

cdR :: Eq t => Transform t a
cdR (d :? (Dim b@(d' := ts) a : ds)) | d /= d'
    , Just as <- mergeMaybes (map ai ds)
    = Just (Dim b (d :? (a:as)))
  where ai (Dim c e) | b == c = Just e
        ai _ = Nothing
cdR _ = Nothing

ccsR :: (Eq t, Eq a) => Int -> Transform t a
ccsR i (d :? ((d' :? as):cs)) | i < length as
                              , Just as' <- mergeMaybes (map ai cs) =
    Just (d' :? (h ++ (d :? (a:as')) : t))
  where (h, a:t) = splitAt i as
        ai (c :? bs) | (g,b:s) <- splitAt i bs, (d',h,t) == (c,g,s) = Just b
        ai _ = Nothing
ccsR _ _ = Nothing

ccmR :: Int -> Transform t a
ccmR i (d :? as) | (h, (c :? bs):t) <- splitAt i as, c == d, i < length bs =
    Just (d :? (h ++ (bs !! i) : t))
ccmR _ _ = Nothing

--
-- Other rules: left-to-right
--

-- Introduce an irrelevant dimension.
dnewL :: Name -> [t] -> Transform t a
dnewL d ts e | notElem d (fd e) = Just (Dim (d := ts) e)
             | otherwise        = Nothing

-- Introduce an irrelevant let binding.
bnewL :: Name -> Expr t a -> Transform t a
bnewL v e' e | notElem v (fv e) = Just (Let (v := e') e)
             | otherwise        = Nothing

-- TODO: Seems like it needs a more cryptic name... :)
namingL :: Name -> Transform t a
namingL v e = Just (Let (v := e) (Var v))

buuL :: Transform t a
buuL (Let (v := e) (Let (w := f) g))
  | notElem v (fv f), notElem w (fv e) = Just (Let (w := f) (Let (v := e) g))
buuL _ = Nothing

--
-- Other rules: right-to-left
--

-- Remove an irrelevant dimension.
dnewR :: Transform t a
dnewR (Dim (d := _) e) | notElem d (fd e) = Just e
dnewR _ = Nothing

-- Remove an irrelevant let binding.
bnewR :: Transform t a
bnewR (Let (v := _) e) | notElem v (fv e) = Just e
bnewR _ = Nothing

namingR :: Transform t a
namingR (Let (v := e) (Var w)) | v == w = Just e
namingR _ = Nothing

buuR :: Transform t a
buuR = buuL

-----------------------
-- Utility Functions --
-----------------------

-- Return a list of free dimension names in e.
fd :: Expr t a -> [Name]
fd (Dim (d := _) e) = delete d (fd e)
fd (d :? as)        = nub (d : concatMap fd as)
fd e                = (nub . concatMap fd . children) e

-- Return a list of free variables in e.
fv :: Expr t a -> [Name]
fv (Let (v := _) e) = delete v (fv e)
fv (Var v)          = [v]
fv e                = (nub . concatMap fv . children) e

mergeMaybes :: [Maybe a] -> Maybe [a]
mergeMaybes ms | all isJust ms = Just (map fromJust ms)
               | otherwise     = Nothing

----------------
-- Testing... --
----------------

csE :: Expr Int Int
csE = 1 :< [leaf 2, "A" :? [leaf 3, leaf 4], leaf 5]
csE' = fromJust (csL 1 csE)
csE'' = fromJust (csR 1 csE')

cbdE :: Expr Int Int
cbdE = Let ("v" := ("A" :? [leaf 1, leaf 2, leaf 3])) (Var "v")
cbdE' = fromJust (cbdL cbdE)
cbdE'' = fromJust (cbdR cbdE')

cbdE2 :: Expr Int Int
cbdE2 = Let ("v" := ("A" :? [leaf 1, "B" :? [leaf 2,leaf 3]])) (Var "v")

cbuE :: Expr Int Int
cbuE = Let ("v" := leaf 1) ("A" :? [leaf 2, leaf 3, leaf 4])
cbuE' = fromJust (cbuL cbuE)
cbuE'' = fromJust (cbuR cbuE')

cdE :: Expr Int Int
cdE = Dim ("A" := [1,2]) ("B" :? [leaf 1, leaf 2, leaf 3])
cdE' = fromJust (cdL cdE)
cdE'' = fromJust (cdR cdE')

ccsE :: Expr Int Int
ccsE = "A" :? [leaf 1, "B" :? [leaf 2, leaf 3], leaf 4]
ccsE' = fromJust (ccsL 1 ccsE)
ccsE'' = fromJust (ccsR 1 ccsE')

ccmE :: Expr Int Int
ccmE = "A" :? [leaf 1, leaf 2, leaf 3]
ccmE' = fromJust (ccmL 1 ccmE)
ccmE'' = fromJust (ccmR 1 ccmE')

test = csE == csE''
    && cbdE == cbdE''
    && cbuE == cbuE''
    && cdE == cdE''
    && ccsE == ccsE''
    && ccmE == ccmE''
