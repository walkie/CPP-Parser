
module CPP.Cond where

import Control.Monad (liftM,liftM2)
import Data.Maybe

import CPP.Lang (CExpr(..),Macro,eval)
import qualified CPP.Lang as C (UnOp(..),BinOp(..))

type Name = String

data BExpr = Con Bool
           | Var Name 
           | Not BExpr
           | And BExpr BExpr
           | Or  BExpr BExpr
           deriving (Eq, Show)

data Cond a = CD  a
            | IT  BExpr [Cond a]
            | ITE BExpr [Cond a] [Cond a]
            deriving (Eq, Show)

mcon = Just . Con
mvar = Just . Var
mnot = liftM  Not
mand = liftM2 And
mor  = liftM2 Or

boolize :: CExpr -> Maybe BExpr
boolize (IntConst  i) = mcon (i /= 0)
boolize (CharConst c) = mcon (fromEnum c /= 0)
boolize (Defined   v) = mvar v
boolize (TerIf c t e) = boolize c `mand` boolize t `mor` boolize e
boolize (UnOp  C.Not e)   = mnot (boolize e)
boolize (BinOp C.And l r) = boolize l `mand` boolize r
boolize (BinOp C.Or  l r) = boolize l `mor`  boolize r
boolize _ = Nothing

-- Translating into "simple" conditional expressions.

data Simple a = SD a
              | SC Name [Simple a] [Simple a]
              deriving (Eq, Show)

simplify :: [Cond a] -> [Simple a]
simplify = concatMap simp
  where simp (CD d) = [SD d]
        simp (IT c t) = simp (ITE c t [])
        simp (ITE (Con True)  t e) = simplify t
        simp (ITE (Con False) t e) = simplify e
        simp (ITE (Var v) t e) = [SC v (simplify t) (simplify e)]
        simp (ITE (Not c) t e) = simp (ITE c e t)
        simp (ITE (And c d) t e) = simp (ITE c [ITE d t e] e)
        simp (ITE (Or  c d) t e) = simp (ITE c t [ITE d t e]) -- data duplication!
