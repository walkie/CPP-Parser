{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Main where

import Data.List (intersect,nub,partition,union)

import System.Environment
import System.IO.Unsafe

import Language.CPP.Syntax
import Language.CPP.Parser
import Analysis.Conditional
import Analysis.Translator

parseFiles :: Eq a => KeepData a -> [FilePath] -> IO [File a]
parseFiles k = mapM (unsafeInterleaveIO . parseFile k)

parseAndTranslate :: (Eq a, DataIs a)
                  => KeepData a -> [FilePath] -> IO ([Cond BExpr (StoreAs a)], [CExpr])
parseAndTranslate k ps = parseFiles k ps >>= return . convert . extract

readPathsFromFile :: FilePath -> IO [FilePath]
readPathsFromFile f = readFile f >>= return . lines

mainGetPaths :: IO [FilePath]
mainGetPaths = do as <- getArgs
                  case as of
                    ("-f":[f]) -> readPathsFromFile f
                    _ -> return as

main = do fs <- mainGetPaths >>= parseFiles discard
          putStrLn $ "Total number of macros: " ++ show (length (findMacros fs))
