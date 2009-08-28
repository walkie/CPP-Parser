{-# LANGUAGE TypeSynonymInstances #-}

module Choice where

import Data.Maybe (fromMaybe)
import Data.List  (elemIndex, intersperse)

import Color
import PrintList

type Name = String

data Expr t a = Var Name
              | a :< [Expr t a]
              | Choose [t] [Bind t a] (Expr t a)
              | Expr t a :! t

data Bind t a = Name := [Expr t a]

var :: Bind t a -> Name
var (n := _) = n

alts :: Bind t a -> [Expr t a]
alts (_ := as) = as

-- Static analysis:
-- Check to make sure each choice is well-defined.

check :: Expr t a -> Bool
check (e :! _)  = check e
check (_ :< es) = all check es
check (Choose ts bs e) = all (checkB (length ts)) bs && check e
check _ = True

checkB :: Int -> Bind t a -> Bool
checkB n (_ := as) = length as == n && all check as

-- Big-step semantics:
-- Assumes "local selection".

type Map t a = [(Name, Expr t a)]

eval :: (Eq t, Show t) => Expr t a -> Expr t a
eval = evalWith []

evalWith :: (Eq t, Show t) => Map t a -> Expr t a -> Expr t a
evalWith m v@(Var n) = fromMaybe v (lookup n m)
evalWith m (a :< es) = a :< map (evalWith m) es
evalWith m (Choose ts bs e) = Choose ts (map (evalWithB m) bs) (evalWith m e)
evalWith m (e :! t) = 
  case evalWith m e of
    Choose ts bs body -> 
      case elemIndex t ts of
        Just i  -> evalWith (map (bind i) bs ++ m) body
        Nothing -> error ("Selected tag not in choice: " ++ show t)
    _ -> error ("Selection not applied to choice:\n") -- ++ show e)

bind :: Int -> Bind t a -> (Name, Expr t a)
bind i (n := as) = (n, as !! i)

evalWithB :: (Eq t, Show t) => Map t a -> Bind t a -> Bind t a
evalWithB m (n := as) = n := map (evalWith m) as

--
-- Following adapted from last round of code from Martin.
--

-- smart constructors
--
text :: [Expr t String] -> Expr t String
text = ("":<)

leaf :: a -> Expr t a
leaf = (:<[])

-- turning textual code into an Expr value by extracting choice variables
--
parse :: String -> Expr t String
parse = text . map filterVars . intersperse " " . words
        where filterVars ('@':s) = Var s
              filterVars s       = leaf s

-- example code
--
x = leaf "x"
y = leaf "y"

vv  = parse "@v + @v"
vvv = parse "@v + @v + @v"
cv  = parse "@c + @v"
v2  = parse "2 * @v"
v3  = parse "3 * @v"

twice = Choose ["x","y"] ["v" := [x,y]]
      $ Choose ["+","*"] ["c" := [vv,v2]]
      $ parse "twice @v = @c"

thrice = Choose ["x","y"] ["v" := [x,y]]
       $ Choose ["+","*"] ["c" := [vv,v2], "d" := [vvv,v3]]
       $ text (map parse ["twice @v = @c", "thrice @v = @d"])

thrice2 = Choose ["x","y"] ["v" := [x,y]]
        $ Choose ["+","*"] ["c" := [vv,v2], "d" := [cv,v3]]
        $ text (map parse ["twice @v = @c", "thrice @v = @d"])

-- 
-- Show instances
--

instance (ShowNesting t, ShowNesting a) => Show (Expr t a) where
  show (Var v)   = metaV v
  show (a :< []) = showS a
  show (a :< es) = showS a ++ opAng a ++ showas es ++ clAng a
  show (e :! t)  = show e ++ metaOp " ! " ++ (metaT (showS t))
  show (Choose ts bs e) = 
    metaKey "choose" ++ metaOp " <" ++ 
    showss (map (metaT . showS) ts) ++ 
    metaOp "> : " ++ 
    printList (map metaOp ["",";",""]) show bs ++
    metaKey " in\n" ++ show e

instance (ShowNesting t, ShowNesting a) => Show (Bind t a) where
  show (v := es) = metaV v ++ metaOp " = " ++ 
    printList (map (metaOp . (:[])) "<,>") show es
  
showas :: (ShowNesting t, ShowNesting a) => [Expr t a] -> String
showas xs@(x:_) = printList ["",comma (getA x),""] show xs

getA :: Expr t a -> a
getA (a :< _)                   = a
getA (Var _)                    = error "getA fails on Var"

showss :: [String] -> String
showss = concat . intersperse ","

metaOp  = colorString blue 
metaKey = colorString (blue ++ boldOn)
metaV   = colorString red 
metaT   = colorString green

class Show a => ShowNesting a where
  showS :: a -> String
  opAng :: a -> String
  clAng :: a -> String
  comma :: a -> String
  showS = show
  opAng _ = "<"
  comma _ = ", "
  clAng _ = ">"

instance ShowNesting Char where
  showS = (:[])
  opAng _ = "<"
  comma _ = ","
  clAng _ = ">"

instance ShowNesting String where
  showS = id
  opAng _ = ""
  comma _ = ""
  clAng _ = ""
