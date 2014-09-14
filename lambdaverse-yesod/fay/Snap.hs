{-# LANGUAGE EmptyDataDecls #-}


module Snap where
import           FFI
import           Prelude

type Coordinate = (Int, Int)

data Paper



snapSelector :: String -> Fay Paper
snapSelector = ffi "window['Snap'](%1)"

--

data Element

attr :: Element -> String -> String -> Fay ()
attr = ffi "%1.attr(%2,%3)"

attrPaper :: Paper -> String -> String -> Fay ()
attrPaper = ffi "%1.attr(%2,%3)"

attrValue :: Element -> String -> String
attrValue = ffi "%1.attr(%2)"

path' :: String -> Paper -> Fay Element
path' = ffi "%2.path(%1)"

path :: String -> Paper -> Fay Element
path spec p = do
  p <- path' spec p
--  attr p "fill" "none"
--  attr p "stroke" "grey"
  attr p "class" "lvHexagon"
  return p


group :: Paper -> [Element] -> Fay Element
group p es = do
  g <- group' p
  let groupAdd :: Element -> Element -> Fay ()
      groupAdd = ffi "%1.add(%2)"
  _ <- sequence $ map (groupAdd g) es
  return g

group' :: Paper -> Fay Element
group' = ffi "%1.g()"


paperText :: Paper -> Int -> Int -> String -> Fay Element
paperText = ffi "%1.text(%2, %3, %4)"

data PathCommand =
  MoveTo (Int, Int) |
  ClosePath |
  LineTo (Int, Int)

renderPath :: [PathCommand] -> String
renderPath [] = ""
renderPath (c:cs) = renderPathCmd c ++ renderPath cs
renderPathCmd :: PathCommand -> String
renderPathCmd (MoveTo (x, y)) = 'M' : show x ++ "," ++ show y
renderPathCmd ClosePath = "Z"
renderPathCmd (LineTo (x, y)) = 'L' : show x ++ "," ++ show y

pathCommands :: [PathCommand] -> Paper -> Fay Element
pathCommands = path . renderPath


