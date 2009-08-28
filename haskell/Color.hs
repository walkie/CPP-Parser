module Color where


type Color = String


-- bold/underline/...
--
reset   = "\27[0m"
boldOn  = "\27[1m"
-- boldOff = "\27[2m"

-- change foreground color
--
attrFG :: Int -> String
attrFG c = "\27[3" ++ show c ++ "m"


black  = attrFG 0
red    = attrFG 1
green  = attrFG 2
yellow = attrFG 3
blue   = attrFG 4
purple = attrFG 5
cyan   = attrFG 6
white  = attrFG 7

defaultColor = black ++ reset

colorString :: Color -> String -> String
colorString c s = c++s++defaultColor



testColor :: Int -> IO ()
testColor c = putStrLn (attrFG c++"This is color "++show c++black)


-- c>7 => color = black
--
colors = mapM testColor [0..7]

