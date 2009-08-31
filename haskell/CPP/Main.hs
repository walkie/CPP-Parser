{-# LANGUAGE FlexibleInstances #-}
module CPP.Main where

import Data.List (intersect,nub)

import System.Environment
import System.IO.Unsafe

import Choice
import CPP.Lang
import CPP.Parser
import CPP.Translator

parseAndTranslate :: Stored a => LinesTo a -> [FilePath] -> IO (Expr Bool a,[CExpr])
parseAndTranslate f ps = do
    fs <- mapM (unsafeInterleaveIO . parseFile) ps
    let cs = extract f fs
    let (cs',bad) = convert cs
    let e = translate cs'
    return (e,bad)

readPathsFromFile :: FilePath -> IO [FilePath]
readPathsFromFile f = readFile f >>= return . lines

mainGetPaths :: IO [FilePath]
mainGetPaths = do as <- getArgs
                  case as of
                    ("-f":[f]) -> readPathsFromFile f
                    _ -> return as

main = do ps <- mainGetPaths
          (e,bad) <- parseAndTranslate discardText ps
          let ds = dom (dims e)
          let badms = nub (concatMap macros bad)
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

instance ShowNesting Bool
instance ShowNesting (Maybe Int) where
  showS (Just i) = show i
  showS Nothing  = "X"
instance ShowNesting Text where
  showS = show
  opAng _ = ""
  comma _ = ""
  clAng _ = ""
