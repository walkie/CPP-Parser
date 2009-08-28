
module CPP.Translator where

import Control.Monad (liftM2, replicateM)

import Text.ParserCombinators.Parsec hiding (Line)
import Text.ParserCombinators.Parsec.Pos (incSourceLine)

import Choice hiding (Name, parse, text)
import CPP.Lang

type CPPExpr    = Expr Expression Text
type Translator = GenParser Line Int

translate :: Translator a -> Name -> [Line] -> a
translate t n ls = either (error . show) id (runParser t 0 n ls)

newId :: String -> Translator Name
newId v = do i <- getState
             setState (i+1)
             return (v ++ show i)

cppToChoice :: File -> CPPExpr
cppToChoice (File n (Text ls)) = 
    Text [Data title] :< translate parts n ls
  where title = "/* FILE: " ++ n ++ " */"

line :: (Line -> Bool) -> Translator Line
line f = tokenPrim show (\p _ _ -> incSourceLine p 1) 
                   (\l -> if f l then Just l else Nothing)

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

tag :: Line -> Expression
tag (Control (DM Ifdef  m)) = Defined m
tag (Control (DM Ifndef m)) = UnOp Not (Defined m)
tag (Control (DE _      e)) = e
tag (Control (D  Else    )) = IntConst 1

body :: [CPPExpr] -> CPPExpr
body []  = leaf (Text [])
body [e] = e
body es  = Text [] :< es

isIf :: Line -> Bool
isIf (Control (DM Ifdef  _)) = True
isIf (Control (DM Ifndef _)) = True
isIf (Control (DE If     _)) = True
isIf _                       = False

isElif :: Line -> Bool
isElif (Control (DE Elif _)) = True
isElif _                     = False

isElse :: Line -> Bool
isElse (Control (D Else)) = True
isElse _                  = False

isEndif :: Line -> Bool
isEndif (Control (D Endif)) = True
isEndif _                   = False

isConditional :: Line -> Bool
isConditional l = any ($ l) [isIf, isElif, isElse, isEndif]

{-
when :: (a -> Bool) -> (a -> b) -> a -> Maybe b
when p f a = if p a then Just (f a) else Nothing

unless :: (a -> Bool) -> (a -> b) -> a -> Maybe b
unless p = when (not . p)
-}

