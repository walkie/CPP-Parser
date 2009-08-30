
module CPP.Translator where

import Control.Monad (liftM, liftM2, replicateM)
import Text.ParserCombinators.Parsec hiding (Line)

import CPP.Cond

import CPP.Lang hiding (Name, UnOp(..), BinOp(..))
import qualified CPP.Lang as C

--import Choice hiding (parse, text)
--import PointFree

-- First pass: Extract conditional structure.

type Extract a = GenParser Line (ES a)
type LinesTo a = Int -> [Line] -> a
type ES a = (Int, LinesTo a)

keepText :: LinesTo Text
keepText _ = Text

discardText :: LinesTo Int
discardText = const

-- The primitive line parser.  Match a line for which the predicate is true.
line :: (Line -> Bool) -> GenParser Line s Line
line f = tokenPrim show (\p _ _ -> incSourceLine p 1) 
                   (\l -> if f l then Just l else Nothing)

-- Match a block of lines at the same nesting level.
block :: Extract a [Cond CExpr a]
block = (liftM2 (:) text block)
    <|> (liftM2 (:) cond block)
    <|> return []

-- Match one or more non-conditional lines.
text :: Extract a (Cond c a)
text = do ls <- many1 (line (not . isConditional))
          (i,f) <- getState
          setState (i+1,f)
          return (CD (f i ls))

-- Match a conditional expression.
cond :: Extract a (Cond CExpr a)
cond = do ifL   <- condLine isIf
          thenB <- block
          elifs <- many (liftM2 (,) (condLine isElif) block)
          melse <- option [] (line isElse >> block)
          return (buildCond ifL thenB elifs melse)
  where condLine f = liftM condition (line f)

buildCond :: CExpr -> [Cond CExpr a] -> 
             [(CExpr, [Cond CExpr a])] -> [Cond CExpr a] -> Cond CExpr a
buildCond c t [] [] = IT  c t
buildCond c t [] e  = ITE c t e
buildCond c t ((eic,eit):eis) e = ITE c t [buildCond eic eit eis e]

{-
type CPPExpr    = Expr CExpr Text
type Translator = GenParser Line TS

data TS = TS Int [Name]

initTS :: TS
initTS = undefined

runTranslator :: Translator a -> TS -> Name -> [Line] -> (a, TS)
runTranslator t = either (error . show) id `o3` runParser t'
  where t' = liftM2 (,) t getState

translate :: Translator a -> Name -> [Line] -> a
translate t = fst `o2` runTranslator t initTS
-}

{-
newId :: String -> Translator Name
newId v = do i <- getState
             setState (i+1)
             return (v ++ show i)

cppToChoice :: [File] -> CPPExpr
cppToChoice fs = concatMap 

files :: [File] -> Translator CPPExpr

file :: Name -> Translator CPPExpr
file n = do setPosition (newPos n 0 0)
            ps <- parts
            return (Text [Data title] :< ps)
  where title = "/* FILE: " ++ n ++ " */"

parts :: Translator [CPPExpr]
parts = (liftM2 (:) text parts)
    <|> (liftM2 (:) cond parts)
    <|> return []

text :: Translator CPPExpr
text = do ls <- many1 (line (not . isConditional))
          return (leaf (Text ls))

cond :: Translator CPPExpr
cond = do ifL <- line isIf
          thenB <- parts
          elifs <- many (block isElif)
          melse <- option (Control (D Else), []) (block isElse)
          line isEndif
          rest  <- parts
          let elses = elifs ++ [melse]
          let tags  = tag ifL : map (tag . fst) elses
          let subs  = body thenB : map (body . snd) elses
          id <- newId "v"
          return (Choose tags [id := subs] (body rest))

block :: (Line -> Bool) -> Translator (Line, [CPPExpr])
block p = liftM2 (,) (line p) parts
-}

{-
body :: [CPPExpr] -> CPPExpr
body []  = leaf (Text [])
body [e] = e
body es  = Text [] :< es
-}

{-
when :: (a -> Bool) -> (a -> b) -> a -> Maybe b
when p f a = if p a then Just (f a) else Nothing

unless :: (a -> Bool) -> (a -> b) -> a -> Maybe b
unless p = when (not . p)
-}

