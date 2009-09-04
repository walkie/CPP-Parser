{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeFamilies #-}
module Main where

import Data.List (intersect,nub)

import System.Environment
import System.IO.Unsafe

import CPP.Lang
import CPP.Parser
import CPP.Translator

import Choice
import Transform

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

instance ShowNesting Bool
instance ShowNesting (Maybe Int) where
  showS (Just i) = show i
  showS Nothing  = "X"
instance ShowData a => ShowNesting (Text a) where
  showS = show
  opAng _ = ""
  comma _ = ""
  clAng _ = ""
