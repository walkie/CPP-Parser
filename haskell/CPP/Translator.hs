
module CPP.Translator where

import Control.Monad (liftM, liftM2, replicateM)
import Text.ParserCombinators.Parsec hiding (Line)

import CPP.Cond

import CPP.Lang hiding (Name, UnOp(..), BinOp(..))
import qualified CPP.Lang as C

--import Choice hiding (parse, text)
--import PointFree

--
-- Phase 1: Extract conditional structure from parsed CPP.
--

type Extract a = GenParser Line (ES a)
type LinesTo a = Int -> [Line] -> a
type ES a = (Int, LinesTo a)

-- Execute this phase of translation.
extract :: LinesTo a -> [File] -> [Cond CExpr a]
extract l fs = either (error . show) id $
               runParser block (0,l) "" (concatMap fileText fs)
  where fileText (File n (Text ls)) = Data ("/* FILE: " ++ n ++ " */") : ls

-- The following two functions are used to indicate whether the controlled text
-- should be retained, or whether this data should be replaced with integer ids.

keepText :: LinesTo Text
keepText _ = Text

discardText :: LinesTo Int
discardText = const

-- The primitive line parser.  Match a line for which the predicate is true.
line :: (Line -> Bool) -> Extract a Line
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
          return (build ifL thenB elifs melse)
  where condLine f = liftM condition (line f)
        build c t [] [] = IT  c t
        build c t [] e  = ITE c t e
        build c t ((eic,eit):eis) e = ITE c t [build eic eit eis e]
