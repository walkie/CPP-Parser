
module CPP.Cond where

import Control.Monad (liftM,liftM2)
import Data.List (nub)

import CPP.Lang (CExpr(..))
import qualified CPP.Lang as C

-- Conditional expressions.

type Name = String

data Cond c a = CD  a
              | IT  c [Cond c a]
              | ITE c [Cond c a] [Cond c a]
              deriving (Eq, Show)

-------------------------
-- Boolean Expressions --
-------------------------

data BExpr = Con Bool
           | Var Name 
           | Not BExpr
           | And BExpr BExpr
           | Or  BExpr BExpr
           deriving (Eq, Show)

-- Some shortcuts for creating Maybe BExprs.
mcon = Just . Con
mvar = Just . Var
mnot = liftM  Not
mand = liftM2 And
mor  = liftM2 Or

-- Convert a C-Expression into a boolean expression, if possible.
boolize :: CExpr -> Maybe BExpr
boolize (IntConst  i) = mcon (i /= 0)
boolize (CharConst c) = mcon (fromEnum c /= 0)
boolize (Defined   v) = mvar v
boolize (TerIf c t e) = boolize c `mand` boolize t `mor` boolize e
boolize (UnOp  C.Not e)   = mnot (boolize e)
boolize (BinOp C.And l r) = boolize l `mand` boolize r
boolize (BinOp C.Or  l r) = boolize l `mor`  boolize r
boolize _ = Nothing

-- Extract all variable names from a boolean expression.
exprVars :: BExpr -> [Name]
exprVars (Con _) = []
exprVars (Var v) = [v]
exprVars (Not e) = exprVars e
exprVars (And e f) = nub $ exprVars e ++ exprVars f
exprVars (Or  e f) = nub $ exprVars e ++ exprVars f

-- Extract all of the conditions from a conditional expression.
conditions :: Cond c a -> [c]
conditions (CD _) = []
conditions (IT c t) = c : concatMap conditions t
conditions (ITE c t e) = c : concatMap conditions (t ++ e)

-- Extract all variable names from a list of conditional expression.
condVars :: [Cond BExpr a] -> [Name]
condVars = nub . concatMap exprVars . concatMap conditions

--
-- Translating into "simple" conditional expressions.
--

data Simple a = SD a
              | SC Name [Simple a] [Simple a]
              deriving (Eq, Show)

simplify :: [Cond BExpr a] -> [Simple a]
simplify = concatMap simp
  where simp (CD d) = [SD d]
        simp (IT c t) = simp (ITE c t [])
        simp (ITE (Con True)  t e) = simplify t
        simp (ITE (Con False) t e) = simplify e
        simp (ITE (Var v) t e) = [SC v (simplify t) (simplify e)]
        simp (ITE (Not c) t e) = simp (ITE c e t)
        simp (ITE (And c d) t e) = simp (ITE c [ITE d t e] e) -- data duplication!
        simp (ITE (Or  c d) t e) = simp (ITE c t [ITE d t e]) -- data duplication!
