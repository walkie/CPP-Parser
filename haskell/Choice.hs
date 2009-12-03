{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Choice where

import Data.List (elemIndex, intersperse, nub)
import Maybe (fromMaybe)

import Color
import PrintList

type Name = String

data Expr t a = 
    Var Name                         -- variable reference
  | a :< [Expr t a]                  -- object branching
  | Let (Bind (Expr t a)) (Expr t a) -- let expression
  | Dim (Bind [t]) (Expr t a)        -- dimension declaration
  | Name :? [Expr t a]               -- choice
    deriving Eq

data Bind a = Name := a
              deriving Eq

var :: Bind a -> Name
var (n := _) = n

val :: Bind a -> a
val (_ := a) = a

children :: Expr t a -> [Expr t a]
children (Var _)   = []
children (_ :< es) = es
children (Let b e) = val b : [e]
children (Dim _ e) = [e]
children (_ :? es) = es

-- Apply a transformation function to all children.
cmap :: (Expr t a -> Expr t a) -> Expr t a -> Expr t a
cmap f v@(Var _) = v
cmap f (a :< es) = a :< map f es
cmap f (Let b e) = Let (fmap f b) (f e)
cmap f (Dim b e) = Dim b (f e)
cmap f (n :? es) = n :? map f es

-- Apply a transformation function to an expression, then recursively apply
-- it to all children.
emap :: (Expr t a -> Expr t a) -> Expr t a -> Expr t a
emap f = cmap (emap f) . cmap f

type Map a = [(Name,a)]

dom :: Map a -> [Name]
dom = map fst

type Dims t = Map [t]

type QTag t      = (Name,t)
type Decision t  = [QTag t]
type Variety t a = [(Decision t, Expr t a)]


-- Note:
--
--   Map a is basically the same as [Bind a]
--

-- 
-- Static analysis
-- TODO Global dimension uniqueness check.
--
wellFormed :: Expr t a -> Bool
wellFormed e = ds == nub ds
               where ds = dom (dims e)

dims :: Expr t a -> Dims t
dims (Var _)           = []
dims (_ :< es)         = concatMap dims es
dims (Let (_ := e) e') = dims e++dims e'
dims (Dim (n := ts) e) = (n,ts):dims e
dims (_ :? es)         = concatMap dims es

equiv :: (Eq t, Eq a) => Variety t a -> [[Decision t]]
equiv = map fst . groupRight

groupRight :: Eq b => [(a,b)] -> [([a],b)]
groupRight ps = scan ps []
  where scan []         g = g
        scan ((x,y):ps) g = 
          case break ((==y).snd) g of
              (_, [])        -> scan ps (g++[([x],y)])
              (g1,(xs,_):g2) -> scan ps (g1++(xs++[x],y):g2)



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
checkWith m e = all (checkWith m) (children e)


--
-- Selection and Semantics
-- (assumes check has been performed)
-- 

sem :: (Show t,Eq t) => Expr t a -> Variety t a
sem = variants . expand []

variants :: (Eq t, Show t) =>  Expr t a -> Variety t a
variants e = selectAll (dims e) e

selectAll :: (Eq t, Show t) => Dims t -> Expr t a -> Variety t a
selectAll []          e = [([],e)]
selectAll ((d,ts):ds) e = [((d,t):qs,select d t e') | 
                           (qs,e') <- selectAll ds e, t <- ts]

select :: (Eq t, Show t) => Name -> t -> Expr t a -> Expr t a
select d t (Dim (n := ts) e) | n == d =
  case elemIndex t ts of
    Just i  -> selectIx d i e
    Nothing -> error $ "Invalid selection: " ++ n ++ "." ++ showPlain t
select d t e = emap (select d t) e

selectIx :: Name -> Int -> Expr t a -> Expr t a
selectIx d i (n :? es)          | n == d = selectIx d i (es !! i)
selectIx d i e@(Dim (n := _) _) | n == d = e
selectIx d i e = emap (selectIx d i) e

selectMany :: (Eq t, Show t) => Map t -> Expr t a -> Expr t a
selectMany m e = foldr (uncurry select) e m

expand :: Map (Expr t a) -> Expr t a -> Expr t a
expand m (Var v) = fromMaybe (error ("Undefined var: "++v)) (lookup v m)
expand m (a :< es)         = a :< map (expand m) es
expand m (Let (v := e) e') = expand ((v,expand m e):m) e'
expand m (Dim b e)         = Dim b (expand m e)
expand m (n :? es)         = n :? map (expand m) es


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

nest1 = Dim ("A" := ["a","b"])
      $ Dim ("X" := ["x","y"])
      $ "A" :? [leaf "1", "X" :? [leaf "8", leaf "9"]]

nest2 = Dim ("A" := ["a","b"])
      $ Dim ("X" := ["x","y"])
      $ Let ("v" := ("X" :? [leaf "8", leaf "9"]))
      $ "A" :? [leaf "1", parse "@v"]

noChoice1 = Dim ("A" := ["a","b"]) (leaf "constant")

noChoice2 = Dim ("A" := ["a","b"]) ("A" :? [leaf "1", leaf "1"])

noChoice3 = Dim ("A" := ["a","b"]) 
          $ Let ("v" := ("A" :? [leaf "1", leaf "2"]))
          $ Let ("w" := ("A" :? [leaf "2", leaf "1"]))
          $ "A" :? [parse "@v + @w", parse "@w + @v"]


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
showPlain :: Show a => a -> String
showPlain = filter (/='"') . show

pv :: (ShowNesting a,ShowNesting t, Eq t) => Expr t a -> IO()
pv = putStr . printList ["\n","\n\n","\n\n"] showPair . variants 
     where showPair (qs,e) = asList (\(d,t)->d++"."++showPlain t) qs
                             ++":\n"++show e
psem:: (ShowNesting a,ShowNesting t, Eq t) => Expr t a -> IO()
psem = pv . expand []

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

instance ShowNesting Bool where
  showS = show
  opAng _ = "{"
  comma _ = ","
  clAng _ = "}"

instance ShowNesting Char where
  showS = (:[])
  opAng _ = "{"
  comma _ = ","
  clAng _ = "}"

instance ShowNesting Int where
  showS = show
  opAng _ = "{"
  comma _ = ","
  clAng _ = "}"

instance ShowNesting Integer where
  showS = show
  opAng _ = "{"
  comma _ = ","
  clAng _ = "}"

instance ShowNesting String where
  showS = id
  opAng _ = ""
  comma _ = ""
  clAng _ = ""
