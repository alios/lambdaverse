{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Hexagon where

import           Prelude
#ifdef FAY
import           FFI
import           RaphaelJS
#else
import           Fay.FFI   ()
#endif

import           Data.Data

data Orientation = Horizontal | Vertical
    deriving (Show, Read, Eq, Typeable, Data)

data Hexgrid =
  Hexgrid {
    grid_orientation :: Orientation,
    grid_hex_size    :: Double,
    grid_width       :: Int,
    grid_height      :: Int
    }  deriving (Show, Read, Eq, Typeable, Data)

hexWidth :: Hexgrid -> Double
hexWidth g = hexWidth' (grid_orientation g) (grid_hex_size g)
hexHeight :: Hexgrid -> Double
hexHeight g = hexHeight' (grid_orientation g) (grid_hex_size g)
hexHorizontalDist :: Hexgrid -> Double
hexHorizontalDist g = hexHorizontalDist' (grid_orientation g) (grid_hex_size g)
hexVerticalDist :: Hexgrid -> Double
hexVerticalDist g = hexVerticalDist' (grid_orientation g) (grid_hex_size g)

hexHeight' :: Orientation -> Double -> Double
hexHeight' Horizontal size = size * 2
hexHeight' o@Vertical size = sqrt(3)/2 * (hexWidth' o size)

hexWidth' :: Orientation -> Double -> Double
hexWidth' o@Horizontal size = sqrt(3)/2 * (hexHeight' o size)
hexWidth' Vertical size = size * 2

hexHorizontalDist' :: Orientation -> Double -> Double
hexHorizontalDist' o@Horizontal size = (hexWidth' o size)
hexHorizontalDist' o@Vertical size = 3/4 * (hexWidth' o size)

hexVerticalDist' :: Orientation -> Double -> Double
hexVerticalDist' o@Horizontal size = 3/4 * (hexHeight' o size)
hexVerticalDist' o@Vertical size = (hexHeight' o size)


#ifdef FAY
renderGrid :: Paper -> Hexgrid -> Fay [Element]
renderGrid paper g =
  let gw = fromIntegral $ grid_width g
      gh = fromIntegral $ grid_height g
      dx = hexHorizontalDist g
      dy = hexVerticalDist g
      hw = hexWidth g
      hh = hexHeight g
      render :: Int -> Int -> Fay Element
      render px py =
        let cx = fromIntegral px * dx
            cy = fromIntegral py * dy
        in case (grid_orientation g) of
          Horizontal ->
            if (even $ py)
            then renderHexagon paper g (cx, cy)
            else renderHexagon paper g (cx - (hw / 2) , cy)
          Vertical ->
            if (even $ px)
            then renderHexagon paper g (cx, cy)
            else renderHexagon paper g (cx, cy - (hh / 2))
  in sequence [ render x y
              | x <- [0..gw - 1], y <- [0..gh - 1]]


renderHexagon :: Paper -> Hexgrid -> (Double, Double) -> Fay Element
renderHexagon paper g (center_x, center_y) =
  pathCommands ((map mkArgs [0..5]) ++ [ClosePath]) paper
  where mkArgs i =
          let size = grid_hex_size g
              ofac = case (grid_orientation g) of
                Horizontal -> 0.5
                Vertical -> 0.0
              angle = 2 * pi / 6 * (i + ofac)
              x_i = round $ center_x + size * cos(angle)
              y_i = round $ center_y + size * sin(angle)
              args = (x_i, y_i)
          in if (i == 0) then MoveTo args else LineTo args

setPaperViewBox :: Paper -> Hexgrid -> (Int, Int) -> Double -> Fay ()
setPaperViewBox paper g (x, y) zoom =
  let w = fromIntegral $ (grid_width g) * (round  $ hexWidth g / zoom)
      h = fromIntegral $ (grid_height g) * (round $ hexHeight g / zoom)
  in setViewBox paper x y w h False
#endif
