module CPP.Lang where

import Data.Bits
import Data.List (intersperse)


type Name = String

data File = File Name Text deriving Eq

-- A block of input which may or may not contain directives.
data Text = Text [Line] deriving Eq

-- A line of input, post gluing of slash-lines.
data Line = Data String 
          | Control Directive
          deriving Eq

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
data Include = System Name | Local Name 
  deriving Eq

-- A command to the C-preprocessor.
data Directive =
    D   D 
  | DF  DF  Include
  | DM  DM  Macro
  | DT  DT  Tokens
  | DE  DE  Expression
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

data Expression = 
    Defined Macro 
  | IntConst Int
  | CharConst Char
  | Macro Macro
  | UnOp  UnOp  Expression
  | BinOp BinOp Expression Expression
  | TerIf       Expression Expression Expression
  deriving Eq

data UnOp  = Not | Pos | Neg | Com
  deriving Eq

data BinOp = Add | Sub | Mul | Div | Mod
           | CLT | LEq | CGT | GEq 
           | CEq | NEq | And | Or
           | ShL | ShR | And'| Or' | Xor
  deriving Eq

-- Expression evaluation

eval :: Expression -> Int
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

evalBinOp :: BinOp -> Int -> Expression -> Int
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
strict :: (Int -> Int -> a) -> Int -> Expression -> a
strict o i e = i `o` eval e

-- For short circuiting operators.
onZero :: (Expression -> a) -> (Expression -> a) -> Int -> Expression -> a
onZero z _ 0 e = z e
onZero _ n _ e = n e

---------------------
-- Pretty printing --
---------------------

sep :: [String] -> String
sep = concat . intersperse " "

paren :: Show a => a -> String
paren a = "(" ++ show a ++ ")"

instance Show Line where
  show (Data s)    = s
  show (Control d) = show d

instance Show Text where
  show (Text ls) = unlines (map show ls)

instance Show File where
  show (File p t) = name ++ show t
    where name = unlines ['/':stars, " * "++p++" *", ' ':stars++"/"]
          stars = replicate (length p + 4) '*'

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
  show (Local n) = "\"" ++ n ++ "\""

instance Show Expression where
  show (Defined m)   = "defined " ++ m
  show (IntConst i)  = show i
  show (CharConst c) = show c
  show (Macro m)     = m
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
