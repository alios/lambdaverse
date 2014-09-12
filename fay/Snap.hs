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
path' :: String -> Paper -> Fay Element
path' = ffi "%2.path(%1)"

path spec p = do
  p <- path' spec p
  attr p "fill" "none"
  attr p "stroke" "grey"
  return p

attr :: Element -> String -> String -> Fay ()
attr = ffi "%1.attr(%2,%3)"


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


