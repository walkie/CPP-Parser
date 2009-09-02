{-# LANGUAGE TypeSynonymInstances #-}

module CPP.Lang where

import Data.Bits
import Data.List (intersperse,nub)


type Name = String

data File a = File Name (Text a) deriving Eq

-- A block of input which may or may not contain directives.
data Text a = Text [Line a] deriving Eq

-- A line of input, post gluing of slash-lines.
data Line a = Data a
            | Control Directive
            deriving Eq

fileName :: File a -> Name
fileName (File n _) = n

fileText :: File a -> Text a
fileText (File _ t) = t

textLines :: Text a -> [Line a]
textLines (Text ls) = ls

----------------
-- Directives --
----------------

-- Macros are strings composed only of letters, numbers and underscores.
-- They are also called "predicates" in the context of assertions.
type Macro = String

-- A type used to capture the "rest of a line".  CPP performs tokenization
-- on these values, but we're ignoring that for now.
type Tokens = String

-- A file argument to the #include family of directives.
data Include = System Name | Local Name | IMacro Macro
  deriving Eq

-- A command to the C-preprocessor.
data Directive =
    D   D 
  | DF  DF  Include
  | DM  DM  Macro
  | DT  DT  Tokens
  | DE  DE  CExpr
  | DMT DMT Macro Tokens -- TODO handle function macros separately?
  | NonStandard Name Tokens
  deriving Eq

data D   = Else    | Endif   | Null               deriving Eq
data DF  = Include | Import  | IncludeNext        deriving Eq
data DM  = Ifdef   | Ifndef  | Undef   | Unassert deriving Eq
data DT  = Error   | Warning | Line    | Pragma   deriving Eq
data DE  = If      | Elif                         deriving Eq
data DMT = Define  | Assert                       deriving Eq

-----------------
-- Expressions --
-----------------

data CExpr = 
    Defined Macro 
  | IntConst Int
  | CharConst Char
  | Macro Macro
  | MFun Macro [CExpr]
  | UnOp  UnOp  CExpr
  | BinOp BinOp CExpr CExpr
  | TerIf       CExpr CExpr CExpr
  deriving Eq

data UnOp  = Not | Pos | Neg | Com
  deriving Eq

data BinOp = Add | Sub | Mul | Div | Mod
           | CLT | LEq | CGT | GEq 
           | CEq | NEq | And | Or
           | ShL | ShR | And'| Or' | Xor
  deriving Eq

macros :: CExpr -> [Macro]
macros (Defined m) = [m]
macros (Macro   m) = [m]
macros (UnOp  _ e)   = nub (macros e)
macros (BinOp _ e f) = nub (concatMap macros [e,f])
macros (TerIf e f g) = nub (concatMap macros [e,f,g])
macros _ = []

-- Expression evaluation

eval :: CExpr -> Int
eval (Defined m)   = undefined -- 0 if undefined, 1 if defined
eval (IntConst i)  = i
eval (CharConst c) = fromEnum c
eval (UnOp  o e)   = evalUnOp  o (eval e)
eval (BinOp o l r) = evalBinOp o (eval l) r
eval (TerIf c t e) | eval c /= 0 = eval t 
                   | otherwise   = eval e

evalUnOp :: UnOp -> Int -> Int
evalUnOp Not 0 = 1
evalUnOp Not _ = 0
evalUnOp Pos n = n
evalUnOp Neg n = -n
evalUnOp Com n = complement n

evalBinOp :: BinOp -> Int -> CExpr -> Int
-- Arithmetic
evalBinOp Add = strict (+)
evalBinOp Sub = strict (-)
evalBinOp Mul = strict (*)
evalBinOp Div = strict div
evalBinOp Mod = strict mod
-- Comparison
evalBinOp CLT = fromEnum `dot` strict (<)
evalBinOp LEq = fromEnum `dot` strict (<=)
evalBinOp CGT = fromEnum `dot` strict (>)
evalBinOp GEq = fromEnum `dot` strict (>=)
evalBinOp CEq = fromEnum `dot` strict (==)
evalBinOp NEq = fromEnum `dot` strict (/=)
-- Logical
evalBinOp And = onZero (const 0) (evalBinOp NEq 0)
evalBinOp Or  = onZero (evalBinOp CEq 0) (const 1)
-- Bit-wise
evalBinOp ShL = strict shiftL
evalBinOp ShR = strict shiftR
evalBinOp And'= strict (.&.)
evalBinOp Or' = strict (.|.)
evalBinOp Xor = strict xor

dot :: (c -> d) -> (a -> b -> c) -> a -> b -> d
dot = ((.).(.))

-- For strict operators.
strict :: (Int -> Int -> a) -> Int -> CExpr -> a
strict o i e = i `o` eval e

-- For short circuiting operators.
onZero :: (CExpr -> a) -> (CExpr -> a) -> Int -> CExpr -> a
onZero z _ 0 e = z e
onZero _ n _ e = n e

---------------
-- Functions --
---------------

-- Predicates for identifying conditional directives.

isIf :: Line a -> Bool
isIf (Control (DM Ifdef  _)) = True
isIf (Control (DM Ifndef _)) = True
isIf (Control (DE If     _)) = True
isIf _                       = False

isElif :: Line a -> Bool
isElif (Control (DE Elif _)) = True
isElif _                     = False

isElse :: Line a -> Bool
isElse (Control (D Else)) = True
isElse _                  = False

isEndif :: Line a -> Bool
isEndif (Control (D Endif)) = True
isEndif _                   = False

isConditional :: Line a -> Bool
isConditional l = any ($ l) [isIf, isElif, isElse, isEndif]

-- Extract the condition from a conditional directive.
condition :: ShowData a => Line a -> CExpr
condition (Control (DM Ifdef  m)) = Defined m
condition (Control (DM Ifndef m)) = UnOp Not (Defined m)
condition (Control (DE _      e)) = e
condition (Control (D  Else    )) = IntConst 1
condition l = error $ "Not a conditional directive: " ++ show l

---------------------
-- Pretty printing --
---------------------

sep :: [String] -> String
sep = concat . intersperse " "

paren :: Show a => a -> String
paren a = "(" ++ show a ++ ")"

class Show a => ShowData a where
  showData :: a -> String
  showData = show

instance ShowData String where
  showData s = s

instance ShowData ()

instance ShowData a => Show (Line a) where
  show (Data a)    = showData a
  show (Control d) = show d

instance ShowData a => Show (Text a) where
  show (Text ls) = unlines (map show ls)

instance ShowData a => Show (File a) where
  show (File p t) = name ++ show t
    where name = "/** GREPME File: " ++ p ++ " */"

instance Show Directive where
  show (D d) = show d
  show (DF d f) = sep [show d, show f]
  show (DM d m) = sep [show d, m]
  show (DT d t) = sep [show d, t]
  show (DE d e) = sep [show d, show e]
  show (DMT d m t) = sep [show d, m, t]
  show (NonStandard n t) = sep ['#':n, t]

instance Show Include where
  show (System n) = "<" ++ n ++ ">"
  show (Local  n) = "\"" ++ n ++ "\""
  show (IMacro m) = m

instance Show CExpr where
  show (Defined m)   = "defined " ++ m
  show (IntConst i)  = show i
  show (CharConst c) = show c
  show (Macro m)     = m
  show (MFun m as)   = m ++ (paren . concat . intersperse "," . map show) as
  show (UnOp o e)    = show o ++ paren e
  show (BinOp o l r) = sep [paren l, show o, paren r]
  show (TerIf c t e) = sep [paren c, "?", paren t, ":", paren e]

instance Show D where
  show Else  = "#else"
  show Endif = "#endif"
  show Null  = "#"

instance Show DF where
  show Include     = "#include"
  show IncludeNext = "#include_next"
  show Import      = "#import"

instance Show DM where
  show Ifdef    = "#ifdef"
  show Ifndef   = "#ifndef"
  show Undef    = "#undef"
  show Unassert = "#unassert"

instance Show DT where
  show Error   = "#error"
  show Warning = "#warning"
  show Line    = "#line"
  show Pragma  = "#pragma"

instance Show DE where
  show If   = "#if"
  show Elif = "#elif"

instance Show DMT where
  show Define = "#define"
  show Assert = "#assert"

instance Show UnOp where
  show Not = "!"
  show Pos = "+"
  show Neg = "-"
  show Com = "~"

instance Show BinOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Mod = "%"
  show CLT = "<"
  show LEq = "<="
  show CGT = ">"
  show GEq = ">="
  show CEq = "=="
  show NEq = "!="
  show And = "&&"
  show Or  = "||"
  show ShL = "<<"
  show ShR = ">>"
  show And'= "&"
  show Or' = "|"
  show Xor = "^"
