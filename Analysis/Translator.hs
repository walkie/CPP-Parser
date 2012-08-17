{-# LANGUAGE TypeFamilies, FlexibleInstances, TypeSynonymInstances #-}

module Analysis.Translator where

import Control.Monad.State
import Data.Maybe (catMaybes)
import Text.Parsec hiding (Line, State)

import Analysis.Conditional

import Language.CPP.Syntax hiding (Name, UnOp(..), BinOp(..))

--
-- * Phase 1: Extract conditional structure from parsed CPP.
--

type Extract a = Parsec [Line a] Int

-- | Execute this phase of translation.
extract :: DataIs a => [File a] -> [Cond CExpr (StoreAs a)]
extract fs = either (error . show) id $
             runParser top 0 "" (concatMap (textLines . fileText) fs)
  --where fileText (File n (Text ls)) = Data ("/* GREPME File: " ++ n ++ " */") : ls

-- | The following two classes are used to determine whether the controlled text
--   should be retained, or whether this data should be replaced with integer ids.

class ShowData a => DataIs a where
  type StoreAs a
  storeData :: Int -> [Line a] -> StoreAs a

instance DataIs String where
  type StoreAs String = Text String
  storeData _ = Text

instance DataIs () where
  type StoreAs () = Maybe Int
  storeData _ [] = Nothing
  storeData i _  = Just i

-- | The primitive line parser.  Match a line for which the predicate is true.
line :: DataIs a => (Line a -> Bool) -> Extract a (Line a)
line f = tokenPrim show (\p _ _ -> incSourceLine p 1) 
                   (\l -> if f l then Just l else Nothing)

top :: DataIs a => Extract a [Cond CExpr (StoreAs a)]
top = do b <- block 
         case b of
           [] -> (line isEndif >> top) -- GCC allows extra #endifs...
                 <|> (eof >> return [])
           _  -> liftM (b ++) top

-- | Match a block of lines at the same nesting level.
block :: DataIs a => Extract a [Cond CExpr (StoreAs a)]
block = (liftM2 (:) text block)
    <|> (liftM2 (:) cond block)
    <|> return []

-- | Match one or more non-conditional lines.
text :: DataIs a => Extract a (Cond c (StoreAs a))
text = do ls <- many1 (line (not . isConditional))
          i  <- getState
          setState (i+1)
          return (Leaf (storeData i ls))

-- | Match a conditional expression.
cond :: DataIs a => Extract a (Cond CExpr (StoreAs a))
cond = do ifL   <- condLine isIf
          thenB <- block
          elifs <- many (liftM2 (,) (condLine isElif) block)
          melse <- option [] (line isElse >> block)
          line isEndif
          return (build ifL thenB elifs melse)
  where condLine f = liftM condition (line f)
        build c t [] [] = IfThen c t
        build c t [] e  = IfThenElse c t e
        build c t ((eic,eit):eis) e = IfThenElse c t [build eic eit eis e]

--
-- * Phase 2: Convert C-expressions into boolean expressions,
--            removing untranslateable branches.
--

type Convert = State [CExpr]

convert :: [Cond CExpr a] -> ([Cond BExpr a], [CExpr])
convert cs = runState (convConds cs) []

convConds :: [Cond CExpr a] -> Convert [Cond BExpr a]
convConds cs = liftM catMaybes (mapM convCond cs)

convCond :: Cond CExpr a -> Convert (Maybe (Cond BExpr a))
convCond (Leaf a)           = (return . Just . Leaf) a
convCond (IfThen c t)       = do t' <- convConds t
                                 fmap2 (\b -> IfThen b t') (convExpr c)
convCond (IfThenElse c t e) = do t' <- convConds t
                                 e' <- convConds e
                                 fmap2 (\b -> IfThenElse b t' e') (convExpr c)

convExpr :: CExpr -> Convert (Maybe BExpr)
convExpr c = maybe (modify (c:) >> return Nothing) (return . Just) (boolize c)

fmap2 = fmap . fmap
