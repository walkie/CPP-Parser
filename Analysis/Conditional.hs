
-- | A simpler representation of conditional structures.  We translate 
--   CPP-annotated code into this for easier analysis.
module Analysis.Conditional where

import Control.Monad (liftM,liftM2)
import Data.List (nub)

import Language.CPP.Syntax (CExpr(..))
import qualified Language.CPP.Syntax as C

--
-- * Conditional expressions
--

type Name = String

data Cond c a = Leaf a
              | IfThen     c [Cond c a]
              | IfThenElse c [Cond c a] [Cond c a]
              deriving (Eq, Show)

--
-- * Boolean Expressions
--

data BExpr = Con Bool
           | Var Name 
           | Not BExpr
           | And BExpr BExpr
           | Or  BExpr BExpr
           deriving (Eq, Show)

-- | Some shortcuts for creating Maybe BExprs.
mcon = Just . Con
mvar = Just . Var
mnot = liftM  Not
mand = liftM2 And
mor  = liftM2 Or

-- | Convert a C-Expression into a boolean expression, if possible.
boolize :: CExpr -> Maybe BExpr
boolize (IntConst  i) = mcon (i /= 0)
boolize (CharConst c) = mcon (fromEnum c /= 0)
boolize (Defined   v) = mvar v
boolize (Macro     m) = mvar m
boolize (TerIf c t e) = boolize c `mand` boolize t `mor` boolize e
boolize (UnOp  C.Not e)   = mnot (boolize e)
boolize (BinOp C.And l r) = boolize l `mand` boolize r
boolize (BinOp C.Or  l r) = boolize l `mor`  boolize r
boolize (BinOp C.CEq (Macro m) (IntConst 0)) = mnot (mvar m)
boolize (BinOp C.CEq (IntConst 0) (Macro m)) = mnot (mvar m)
boolize (BinOp C.NEq (Macro m) (IntConst 0)) = mvar m
boolize (BinOp C.NEq (IntConst 0) (Macro m)) = mvar m
boolize _ = Nothing

-- | Extract all variable names from a boolean expression.
exprVars :: BExpr -> [Name]
exprVars (Con _) = []
exprVars (Var v) = [v]
exprVars (Not e) = exprVars e
exprVars (And e f) = nub $ exprVars e ++ exprVars f
exprVars (Or  e f) = nub $ exprVars e ++ exprVars f

-- | Extract all of the conditions from a conditional expression.
conditions :: Cond c a -> [c]
conditions (Leaf _) = []
conditions (IfThen c t) = c : concatMap conditions t
conditions (IfThenElse c t e) = c : concatMap conditions (t ++ e)

-- | Extract all variable names from a list of conditional expression.
condVars :: [Cond BExpr a] -> [Name]
condVars = nub . concatMap exprVars . concatMap conditions

--
-- * Translating into "simple" conditional expressions.
--

data Simple a = SLeaf a
              | SCond Name [Simple a] [Simple a]
              deriving (Eq, Show)

simplify :: [Cond BExpr a] -> [Simple a]
simplify = concatMap simp
  where simp (Leaf d) = [SLeaf d]
        simp (IfThen c t) = simp (IfThenElse c t [])
        simp (IfThenElse (Con True)  t e) = simplify t
        simp (IfThenElse (Con False) t e) = simplify e
        simp (IfThenElse (Var v) t e) = [SCond v (simplify t) (simplify e)]
        simp (IfThenElse (Not c) t e) = simp (IfThenElse c e t)
        simp (IfThenElse (And c d) t e) = simp (IfThenElse c [IfThenElse d t e] e) -- data duplication!
        simp (IfThenElse (Or  c d) t e) = simp (IfThenElse c t [IfThenElse d t e]) -- data duplication!
