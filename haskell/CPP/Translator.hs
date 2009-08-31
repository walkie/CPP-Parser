
module CPP.Translator where

import Control.Monad.State
import Data.Maybe (catMaybes)
import Text.ParserCombinators.Parsec hiding (Line, State)

import CPP.Cond

import CPP.Lang hiding (Name, UnOp(..), BinOp(..))

import Choice hiding (Name, Var, parse, text)
import qualified Choice

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

discardText :: LinesTo (Maybe Int)
discardText _ [] = Nothing
discardText i _  = Just i

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
          line isEndif
          return (build ifL thenB elifs melse)
  where condLine f = liftM condition (line f)
        build c t [] [] = IT  c t
        build c t [] e  = ITE c t e
        build c t ((eic,eit):eis) e = ITE c t [build eic eit eis e]

--
-- Phase 2: Convert C-expressions into boolean expressions,
--          removing untranslateable branches.
--

type Convert = State [CExpr]

convert :: [Cond CExpr a] -> ([Cond BExpr a], [CExpr])
convert cs = runState (convConds cs) []

convConds :: [Cond CExpr a] -> Convert [Cond BExpr a]
convConds cs = liftM catMaybes (mapM convCond cs)

convCond :: Cond CExpr a -> Convert (Maybe (Cond BExpr a))
convCond (CD a)      = (return . Just . CD) a
convCond (IT c t)    = do t' <- convConds t
                          fmap2 (\b -> IT b t') (convExpr c)
convCond (ITE c t e) = do t' <- convConds t
                          e' <- convConds e
                          fmap2 (\b -> ITE b t' e') (convExpr c)

convExpr :: CExpr -> Convert (Maybe BExpr)
convExpr c = maybe (modify (c:) >> return Nothing) (return . Just) (boolize c)

fmap2 = fmap . fmap

--
-- Phase 3: Translate conditional expressions into the choice syntax,
--          simplifying where possible.
--

type Translate = State Int

class    Stored a         where empty :: a
instance Stored Text      where empty = Text []
instance Stored (Maybe a) where empty = Nothing

translate :: Stored a => [Cond BExpr a] -> Expr Bool a
translate cs = foldr ($) body dims
  where dims = map dim (condVars cs)
        body = evalState (tconds cs) 0

newVar :: Translate Name
newVar = do i <- get
            put (i+1)
            return ("v" ++ show i)

exprList :: Stored a => [Expr Bool a] -> Expr Bool a
exprList [e] = e
exprList es  = empty :< es

tconds :: Stored a => [Cond BExpr a] -> Translate (Expr Bool a)
tconds cs = liftM exprList (mapM tcond cs)

dim :: Name -> Expr Bool a -> Expr Bool a
dim n = Dim (n := [True,False])

tcond :: Stored a => Cond BExpr a -> Translate (Expr Bool a)
tcond (CD d) = return (leaf d)
tcond (CRef v) = return (Choice.Var v)
tcond (IT c t) = tcond (ITE c t [])
tcond (ITE (Con True)  t e) = tconds t
tcond (ITE (Con False) t e) = tconds e
tcond (ITE (Var v) t e) = do t' <- tconds t
                             e' <- tconds e
                             return (v :? [t',e'])
tcond (ITE (Not c) t e) = tcond (ITE c e t)
tcond (ITE (And c d) t e) = do v <- newVar
                               b <- liftM (v :=) (tconds e)
                               let e' = [CRef v]
                               let t' = [ITE d t e']
                               liftM (Let b) (tcond (ITE c t' e'))
tcond (ITE (Or  c d) t e) = do v <- newVar
                               b <- liftM (v :=) (tconds t)
                               let t' = [CRef v]
                               let e' = [ITE d t' e]
                               liftM (Let b) (tcond (ITE c t' e'))
