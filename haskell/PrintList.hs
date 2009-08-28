--
--  PrintList.hs  --  Utilities for printing lists
--
module PrintList where
             

import List (intersperse)


----------------------------------------------------------------------
-- PRINT UTILITIES
----------------------------------------------------------------------

newtype Lines a = Lines [a]

instance Show a => Show (Lines a) where
  show (Lines xs) = printList ["","\n",""] show xs

-- asLines :: [a] -> Lines a
-- asLines = Lines


showNQ :: Show a => a -> String
showNQ = filter ('"'/=) . show

indent i l = take (i*l) (repeat ' ')

printList sep f xs = sep!!0++concat (intersperse (sep!!1) (map f xs))++sep!!2

sepBy s = printList ["",s,""] id

asTuple = printList ["(",",",")"]
asSeq   = printList ["",",",""]
asList  = printList ["[",",","]"]
asSet   = printList ["{",",","}"]
asLisp  = printList ["("," ",")"]
asPlain f xs = if null xs then "" else printList [" "," ",""] f xs
asPlain' f xs = if null xs then "" else printList [""," ",""] f xs
asString = printList ["","",""] 
asLines = printList ["","\n",""]
asCases l = printList ["\n"++ind++"   ","\n"++ind++" | ",""] where ind = indent 4 l
asDefs n = printList ["\n"++n,"\n"++n,"\n"]
asParagraphs = printList ["\n","\n\n","\n"]

asSingleton f [x] = f x
asSingleton f xs  = asSet f xs

