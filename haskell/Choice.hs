{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Choice where

import Data.List (elemIndex, intersperse)

import Color
import PrintList

type Name = String

data Expr t a = 
    Var Name                         -- variable reference
  | a :< [Expr t a]                  -- object branching
  | Let (Bind (Expr t a)) (Expr t a) -- let expression
  | Dim (Bind [t]) (Expr t a)        -- dimension declaration
  | Name :? [Expr t a]               -- choice

data Bind a = Name := a

var :: Bind a -> Name
var (n := _) = n

val :: Bind a -> a
val (_ := a) = a

subs :: Expr t a -> [Expr t a]
subs (Var _)   = []
subs (_ :< es) = es
subs (Let b e) = val b : [e]
subs (Dim _ e) = [e]
subs (_ :? es) = es

emap :: (Expr t a -> Expr t a) -> Expr t a -> Expr t a
emap f v@(Var _) = v
emap f (a :< es) = a :< map f es
emap f (Let b e) = Let (fmap f b) (f e)
emap f (Dim b e) = Dim b (f e)
emap f (n :? es) = n :? map f es

type Map v = [(Name,v)]

-- 
-- Static analysis
-- TODO Global dimension uniqueness check.
--

-- Checks to make sure each choice contains the correct number
-- of alternatives.
check :: Expr t a -> Bool
check = checkWith []

checkWith :: Map Int -> Expr t a -> Bool
checkWith m (Dim (n := ts) e) =
  case lookup n m of
    Just _  -> error $ "Non-unique dimension: " ++ n
    Nothing -> checkWith ((n, length ts):m) e
checkWith m (n :? es) =
  case lookup n m of
    Just l  -> length es == l && all (checkWith m) es
    Nothing -> error $ "Undefined dimension: " ++ n
checkWith m e = all (checkWith m) (subs e)

--
-- Selection
-- (assumes check has been performed)
-- 

select :: (Eq t, Show t) => Name -> t -> Expr t a -> Expr t a
select d t (Dim (n := ts) e) | n == d =
  case elemIndex t ts of
    Just i  -> selectIx d i e
    Nothing -> error $ "Invalid selection: " ++ n ++ "." ++ show t
select d t e = emap (select d t) e

selectIx :: Name -> Int -> Expr t a -> Expr t a
selectIx d i (n :? es) | n == d = selectIx d i (es !! i)
selectIx d i e = emap (selectIx d i) e

selectAll :: (Eq t, Show t) => Map t -> Expr t a -> Expr t a
selectAll m e = foldr (uncurry select) e m

{- 
-- An ugly, one-pass selectAll that doesn't use select.
selectAll :: (Eq t, Show t) => Map t -> Expr t a -> Expr t a
selectAll = selectWith []

selectWith :: (Eq t, Show t) => Map Int -> Map t -> Expr t a -> Expr t a
selectWith _ _ v@(Var _) = v
selectWith ixs sel (a :< es) = 
  a :< map (selectWith ixs sel) es
selectWith ixs sel (Let (n := e) f) = 
  Let (n := selectWith ixs sel e) (selectWith ixs sel f)
selectWith ixs sel (Dim (n := ts) e) =
  case lookup n sel of
    Just t  -> 
      case elemIndex t ts of
        Just ix -> selectWith ((n,ix):ixs) sel e
        Nothing -> error $ "Invalid selection: " ++ n ++ "." ++ show t
    Nothing -> selectWith ixs sel e
selectWith ixs sel (n :? es) =
  case lookup n ixs of
    Just ix -> selectWith ixs sel (es !! ix)
    Nothing -> n :? map (selectWith ixs sel) es
-}

--
-- Smart constructors
-- 

text :: [Expr t String] -> Expr t String
text = ("" :<)

leaf :: a -> Expr t a
leaf = (:<[])

-- Turn textual code into Exprs by extracting choice variables.
parse :: String -> Expr t String
parse = text . map filterVars . intersperse " " . words
        where filterVars ('@':s) = Var s
              filterVars s       = leaf s

--
-- Examples
--

twice = Dim ("Par" := ["x","y"])
      $ Dim ("Imp" := ["+","*"])
      $ Let ("p" := ("Par" :? [leaf "x", leaf "y"]))
      $ Let ("i" := ("Imp" :? [parse "@p + @p", parse "2 * @p"]))
      $ parse "twice @p = @i"

--
-- Functor instances
--

instance Functor (Expr t) where
  fmap _ (Var n)   = Var n
  fmap f (a :< es) = f a :< map (fmap f) es
  fmap f (Let b e) = Let (fmap (fmap f) b) (fmap f e)
  fmap f (Dim b e) = Dim b (fmap f e)
  fmap f (n :? es) = n :? map (fmap f) es

instance Functor Bind where
  fmap f (n := a) = n := f a

--
-- Show instances
--

instance (ShowNesting t, ShowNesting a) => Show (Expr t a) where
  show (Var v)   = colVar v
  show (a :< []) = showS a
  show (a :< es) = showS a ++ opAng a ++ showas es ++ clAng a
  show (Let b e) = colKey "let " ++ show b ++ colKey " in\n" ++ show e
  show (d :? es) = showdim d (map show es)
  show (Dim (d := ts) e) = 
    colKey "dim " ++ showdim d (map (colTag . showS) ts) ++
    colKey " in\n" ++ show e

instance Show a => Show (Bind a) where
  show (v := a) = colVar v ++ colOp " = " ++ show a

showdim :: Name -> [String] -> String
showdim d ss = colDim d ++ colOp "<" ++ showss ss ++ colOp ">"

showas :: (ShowNesting t, ShowNesting a) => [Expr t a] -> String
showas xs = printList ["", comma (getA (head xs)), ""] show xs
  where getA :: Expr t a -> a
        getA = undefined

showss :: [String] -> String
showss = concat . intersperse ","

colOp  = colorString blue 
colKey = colorString (blue ++ boldOn)
colVar = colorString red 
colDim = colorString green
colTag = colorString green

class Show a => ShowNesting a where
  showS :: a -> String
  opAng :: a -> String
  clAng :: a -> String
  comma :: a -> String
  showS = show
  opAng _ = "{"
  comma _ = ", "
  clAng _ = "}"

instance ShowNesting Char where
  showS = (:[])
  opAng _ = "{"
  comma _ = ","
  clAng _ = "}"

instance ShowNesting String where
  showS = id
  opAng _ = ""
  comma _ = ""
  clAng _ = ""
