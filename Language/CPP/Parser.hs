
module Language.CPP.Parser where

import Control.Monad (liftM, liftM2)
import Data.Char (isSpace)
import Data.List (dropWhile, group, isPrefixOf)

import Text.ParserCombinators.Parsec hiding (Line)
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Pos (newPos)
import Text.ParserCombinators.Parsec.Token (LanguageDef(..),TokenParser,makeTokenParser)
import qualified Text.ParserCombinators.Parsec.Token as Lex

import CPP.Lang

-- Per the definition, CPP proceeds in four passes:
--   1. Trigraph replacement
--   2. Line splicing
--   3. Tokenization
--   4. Macro expansion and directive handling
--
-- Phase 1 is turned off by default in GCC and seems safe to ignore since it
-- exists only for archaic machines.  It could easily be added if needed.
-- Phase 2 is directly implemented as part of the parser.  Tokenization is not
-- implemented exactly as specified in the CPP specification, but hopefully
-- captures the same valid programs (while discarding invalid ones which CPP
-- would pass through to later stages).  Phase 4 is outside the scope of the 
-- ToSC project.

parseCPP :: Eq a => KeepData a -> FilePath -> String -> File a
parseCPP k p = File p . parseLines k p . splice . lines

parseFile :: Eq a => KeepData a -> FilePath -> IO (File a)
parseFile k p = liftM (parseCPP k p) (readFile p)

-------------------
-- Line Splicing --
-------------------

splice :: [String] -> [String]
splice = glue . group . prep
  where 
    prep = map (dropWhile isSpace . reverse)
    glue = map (concat . map reverse)
    group [] = []
    group ls = case span (isPrefixOf "\\") ls of
                 (as, a:bs) -> (map tail as ++ [a]) : group bs
                 (as, _)    -> [map tail as]
    
-----------
-- Lexer --
-----------

cpp = LanguageDef {
    commentStart    = "/*",
    commentEnd      = "*/",
    commentLine     = "//",
    nestedComments  = False,
    identStart      = letter <|> oneOf "_$",
    identLetter     = alphaNum <|> oneOf "_$",
    opStart         = opLetter cpp,
    opLetter        = oneOf "?:!+-~*/%<>&|^=",
    reservedOpNames = map (:"") "?:!+-~*/%<>&|^" ++ 
                      ["<=",">=","==","!=","&&","||","<<",">>"],
    reservedNames   = ["defined"],
    caseSensitive   = True
}

lexer :: TokenParser ()
lexer = makeTokenParser cpp

charLiteral = Lex.charLiteral lexer
identifier = Lex.identifier lexer
integer = Lex.integer lexer
parens = Lex.parens lexer
reservedOp = Lex.reservedOp lexer
reserved = Lex.reserved lexer
comma = Lex.comma lexer

------------
-- Parser --
------------

type KeepData a = String -> a

keep :: KeepData String
keep = id

discard :: KeepData ()
discard = const ()

parseLines :: Eq a => KeepData a -> FilePath -> [String] -> Text a
parseLines k p ls = Text (map run ils) --(Text . map head . group . map run) ils
  where run = try . parseLine (line k) p
        try = either (error . show) id
        ils = zip [1..] ls

parseLine :: Parser a -> FilePath -> (Int, String) -> Either ParseError a
parseLine p f (l,s) = case parse (setPosition (newPos f l 0) >> p) f s of
    Right a  -> Right a
    Left err -> if (Expect "end of comment") `elem` errorMessages err
                then parseLine p f (l,s++"*/") else Left err

line :: KeepData a -> Parser (Line a)
line k = do sp <- many space
            control <|> text sp
  where control = directive >>= return . Control
        text sp = rest >>= return . Data . k . (sp++)

directive :: Parser Directive
directive = do
    char '#'
    many space
    directive <- identifier <|> string ""
    case directive of
      "else"         -> d Else
      "endif"        -> d Endif
      ""             -> d Null
      "include"      -> df Include
      "import"       -> df Import
      "include_next" -> df IncludeNext
      "ifdef"        -> dm Ifdef
      "ifndef"       -> dm Ifndef
      "undef"        -> dm Undef
      "unassert"     -> dm Unassert
      "error"        -> dt Error
      "warning"      -> dt Warning
      "line"         -> dt Line
      "pragma"       -> dt Pragma
      "if"           -> de If
      "elif"         -> de Elif
      "define"       -> dmt Define
      "assert"       -> dmt Assert
      name           -> liftM (NonStandard name) rest
  where d = return . D
        df d = liftM (DF d) file
        dm d = liftM (DM d) macro
        dt d = liftM (DT d) rest
        de d = liftM (DE d) expr
        dmt d = liftM2 (DMT d) macro rest

macro :: Parser Macro
macro = identifier

rest :: Parser Tokens
rest = many anyToken

file :: Parser Include
file = many space >> (system <|> local <|> imacro)
  where system = filename '<' '>' System
        local  = filename '"' '"' Local
        imacro = liftM IMacro macro
        filename open close f = do
            char open
            n <- many (satisfy (/= close))
            char close
            return (f n)

expr :: Parser CExpr
expr = do e <- expr'
          cond e <|> return e
  where cond c = do reservedOp "?"
                    t <- expr
                    reservedOp ":"
                    e <- expr
                    return (TerIf c t e)

-- Everything except ternary conditional operator
expr' :: Parser CExpr
expr' = buildExpressionParser ops factor
  where pre s o = Prefix (reservedOp s >> return (UnOp o))
        inf s o = Infix (reservedOp s >> return (BinOp o)) AssocLeft
        ops = [[pre "!" Not, pre "+" Pos, pre "-" Neg, pre "~" Com],
               [inf "*" Mul, inf "/" Div, inf "%" Mod],
               [inf "+" Add, inf "-" Sub],
               [inf "<<" ShL, inf ">>" ShR],
               [inf "<"  CLT, inf "<=" LEq, inf ">" CGT, inf ">=" GEq],
               [inf "==" CEq, inf "!=" NEq],
               [inf "&" And'], [inf "^" Xor], [inf "|" Or'],
               [inf "&&" And], [inf "||" Or]]

factor :: Parser CExpr
factor = parens expr <|> defined <|> literal <|> macro'
  where macro' = do m <- macro
                    as <- try (parens args) <|> return []
                    return (if null as then Macro m else MFun m as)
        args = do a <- expr
                  as <- try (comma >> args) <|> return []
                  return (a:as)

defined :: Parser CExpr
defined = reserved "defined" >> (parens macro <|> macro) >>= return . Defined

literal :: Parser CExpr
literal = int <|> char
  where int  = liftM (IntConst . fromInteger) integer
        char = liftM CharConst charLiteral
        
instance Eq Message where
  SysUnExpect m == SysUnExpect n = m == n
  UnExpect    m == UnExpect    n = m == n
  Expect      m == Expect      n = m == n
  Message     m == Message     n = m == n
  _ == _ = False
