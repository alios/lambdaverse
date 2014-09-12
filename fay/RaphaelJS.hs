{-# LANGUAGE EmptyDataDecls #-}


module RaphaelJS where
import           FFI
import           Prelude

type Coordinate = (Int, Int)

data Paper


raphael :: String -> Int -> Int -> Fay Paper
raphael = ffi "window.Raphael(%1, %2, %3)"

--

data Element
path :: String -> Paper -> Fay Element
path = ffi "%2.path(%1)"


-- Paper.setViewBox(x, y, w, h, fit)

setViewBox :: Paper -> Int -> Int -> Int -> Int -> Bool -> Fay ()
setViewBox = ffi "%1.setViewBox(%2, %3, %4, %5)"


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


