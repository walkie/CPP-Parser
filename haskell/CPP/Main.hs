
module CPP.Main where

import System.Environment

import Choice
import CPP.Lang
import CPP.Parser
import CPP.Translator

main = do ps <- getArgs
          fs <- mapM parseFile ps
          let cs = map cppToChoice fs
          mapM_ print cs

instance ShowNesting Expression
instance ShowNesting Text where
  showS _ = ""
  opAng _ = ""
  comma _ = ""
  clAng _ = ""
