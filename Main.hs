{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeFamilies #-}
module Main where

import Data.List (intersect,nub,partition,union)

import System.Environment
import System.IO.Unsafe

import CPP.Lang hiding (Name)
import CPP.Parser
import CPP.Translator

import Choice

parseAndTranslate :: (Eq a, DataIs a, Stored (StoreAs a)) => 
                     KeepData a -> [FilePath] -> 
                     IO (Expr Bool (StoreAs a),[CExpr])
parseAndTranslate k ps = do
    fs <- parseFiles k ps
    let cs = extract fs
    let (cs',bad) = convert cs
    let e = translate cs'
    return (e,bad)

parseFiles :: Eq a => KeepData a -> [FilePath] -> IO [File a]
parseFiles k = mapM (unsafeInterleaveIO . parseFile k)

readPathsFromFile :: FilePath -> IO [FilePath]
readPathsFromFile f = readFile f >>= return . lines

mainGetPaths :: IO [FilePath]
mainGetPaths = do as <- getArgs
                  case as of
                    ("-f":[f]) -> readPathsFromFile f
                    _ -> return as

main = do fs <- mainGetPaths >>= parseFiles discard
          putStrLn $ "Total number of macros: " ++ show (length (macrosFs fs))

{-
main = do ps <- mainGetPaths
          --(e,bad) <- parseAndTranslate keep ps
          (e,bad) <- parseAndTranslate discard ps
          let ds = dom (dims e)
          let badms = nub (concatMap macrosE bad)
          let overlap = ds `intersect` badms
          putStrLn $ "** Could not convert " ++ show (length bad) ++ " conditions:"
          print bad
          putStrLn $ "** Containing " ++ show (length badms) ++ " macros:"
          putStr (unlines badms)
          putStrLn $ "** Which overlaps with " ++ show (length overlap) ++ " dimensions in e:"
          putStr (unlines overlap)
          putStrLn $ "** Number of dimensions in e: " ++ show (length ds)
          putStrLn $ "** Total number of macros: " ++ show (length badms + length ds - length overlap)
          putStrLn "--"
          print e
-}

-----------------------
-- Utility Functions --
-----------------------

varsets :: Expr t a -> [[Name]]
varsets = indySets . expr []
  where expr m (Let (v := e) f) = expr ((v,expr m e):m) f
        expr m (Var v)   = maybe [] id (lookup v m)
        expr m (a :< []) = []
        expr m (a :< es) = exprs m es
        expr m (Dim _ e) = expr m e
        expr m (d :? es) = indySets (add d (exprs m es))
        exprs m = indySets . concatMap (expr m)

add :: a -> [[a]] -> [[a]]
add a [] = [[a]]
add a ss = map (a:) ss

indySets :: Eq a => [[a]] -> [[a]]
indySets []     = []
indySets (s:ss) = 
    case partition (null . intersect s) ss of
      (ns,[]) -> nub s : indySets ns
      (ns,ys) -> indySets (foldr union s ys : ns)


instance ShowNesting (Maybe Int) where
  showS (Just i) = show i
  showS Nothing  = "X"
instance ShowData a => ShowNesting (Text a) where
  showS = show
  opAng _ = ""
  comma _ = ""
  clAng _ = ""
