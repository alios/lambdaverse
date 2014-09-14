{-# LANGUAGE DeriveDataTypeable #-}

module Data.Hexagon.Hexgrid where

import           Data.Data
import           Data.Hexagon.AxialCoordinate
import           Data.Hexagon.CubeCoordinate
import           Data.Hexagon.OffsetCoordinate
import           Data.Hexagon.ScreenCoordinate
import           Data.Hexagon.Types
import           Prelude


data Hexgrid =
  Hexgrid {
    grid_orientation :: Orientation,
    grid_hex_size    :: Double,
    grid_width       :: Int,
    grid_height      :: Int
    }  deriving (Show, Read, Eq, Typeable, Data)


mkGridCoordinate :: Hexgrid -> Int -> Int -> AxialCoordinate
mkGridCoordinate g q r =
  AxialCoordinate { _axial_orientation = grid_orientation g
                  , _axial_q = q
                  , _axial_r = r
                  }


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

hexDim :: Hexgrid -> ScreenCoordinate
hexDim g = ScreenCoordinate (round $ hexWidth g) (round $ hexHeight g)

hexDist :: Hexgrid -> ScreenCoordinate
hexDist g = ScreenCoordinate (round $ hexHorizontalDist g) (round $ hexVerticalDist g)



screen2axial :: Hexgrid -> ScreenCoordinate -> AxialCoordinate
screen2axial g s =
  let  x = ((fromIntegral $ _pixel_x s) - (hw / 2.0)) / hw
       hw = hexWidth g
       z = fromIntegral $ _pixel_y s
       t1 = z / (grid_hex_size g)
       t2 = dfloor $ x + t1
       r = floor $ ((dfloor $ t1 - x) + t2) / 3.0
       q = (floor $ ((dfloor $ 2.0 * x + 1.0) + t2) / 3 ) - r
  in mkGridCoordinate g q r
  where dfloor :: Double -> Double
        dfloor d = fromIntegral ((floor d) :: Int)

screen2offset :: Hexgrid -> Offset -> ScreenCoordinate -> OffsetCoordinate
screen2offset g o s = axial2offset o $ screen2axial g s


offset2screen :: Hexgrid -> OffsetCoordinate -> ScreenCoordinate
offset2screen g c =
  let hw = hexHorizontalDist g
      hh = hexVerticalDist g
      q = fromIntegral $ _offset_q c
      r = fromIntegral $ _offset_r c
      op = if (_offset_offset c == Odd) then odd else even
  in case (_offset_orientation c) of
      Horizontal ->
        ScreenCoordinate { _pixel_y = round $ hh * r
                         , _pixel_x = round $ (hw * q) +
                                     (if (op $ _offset_r c) then 0 else (hexWidth g) / 2)
                         }
      Vertical ->
        ScreenCoordinate { _pixel_x = round $ hw * q
                         , _pixel_y = round $ (hh * r) +
                                     (if (op $ _offset_q c) then 0 else (hexHeight g) / 2)
                         }

axial2screen :: Hexgrid -> Offset -> AxialCoordinate -> ScreenCoordinate
axial2screen g o = offset2screen g . axial2offset o

cube2screen :: Hexgrid -> Offset -> CubeCoordinate -> ScreenCoordinate
cube2screen g o = offset2screen g . cube2offset o


inGrid :: Hexgrid -> OffsetCoordinate -> Bool
inGrid g c =
  let  q = _offset_q c
       r = _offset_r c
  in (q >= 0) && (q < grid_width g) && (r >= 0) && (r < grid_height g)

hexesFromScreenBox :: Hexgrid -> Offset -> ScreenBox -> [OffsetCoordinate]
hexesFromScreenBox g offset box =
  let pos1 = viewbox_pos box
      pos2 = pos1 `plusScreen` viewbox_size box
      a = screen2offset g offset pos1
      b = screen2offset g offset pos2
      qmin = min (_offset_q a) (_offset_q b)
      qmax = max (_offset_q a) (_offset_q b)
      rmin = min (_offset_r a) (_offset_r b)
      rmax = max (_offset_r a) (_offset_r b)
      qs = [qmin..qmax]
      rs = [rmin..rmax]
  in filter (inGrid g) [
    OffsetCoordinate { _offset_offset = offset
                     , _offset_orientation = grid_orientation g
                     , _offset_q = q
                     , _offset_r = r
                     } | r <- rs , q <- qs ]


