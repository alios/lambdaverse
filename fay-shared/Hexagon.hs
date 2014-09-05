{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Hexagon where

import           Prelude
#ifdef FAY
import           FFI
import           RaphaelJS
#else
import           Data.Bits
import           Fay.FFI   ()
#endif

import           Data.Data

data Orientation = Horizontal | Vertical
    deriving (Show, Read, Eq, Typeable, Data)
data Offset = Odd | Even
    deriving (Show, Read, Eq, Typeable, Data)


data ScreenCoordinate =
  ScreenCoordinate { pixel_x :: Int
                   , pixel_y :: Int
} deriving (Show, Read, Eq, Typeable, Data)

data AxialCoordinate =
  AxialCoordinate  { axial_orientation :: Orientation
                   , axial_q           :: Int
                   , axial_r           :: Int
} deriving (Show, Read, Eq, Typeable, Data)

data OffsetCoordinate =
  OffsetCoordinate  { offset_orientation :: Orientation
                    , offset_offset      :: Offset
                    , offset_q           :: Int
                    , offset_r           :: Int
} deriving (Show, Read, Eq, Typeable, Data)


data CubeCoordinate =
  CubeCoordinate  { cube_orientation :: Orientation
                  , cube_x           :: Int
                  , cube_y           :: Int
                  , cube_z           :: Int
} deriving (Show, Read, Eq, Typeable, Data)



data Hexgrid =
  Hexgrid {
    grid_orientation :: Orientation,
    grid_offset      :: Offset,
    grid_hex_size    :: Double,
    grid_width       :: Int,
    grid_height      :: Int
    }  deriving (Show, Read, Eq, Typeable, Data)


cube2axial :: CubeCoordinate -> AxialCoordinate
cube2axial c =
  AxialCoordinate { axial_orientation = cube_orientation c
                  , axial_q = cube_x c
                  , axial_r = cube_z c
                  }

axial2cube :: AxialCoordinate -> CubeCoordinate
axial2cube c =
  CubeCoordinate { cube_orientation = axial_orientation c
                 , cube_x = axial_q c
                 , cube_y = -(axial_q c) - (axial_r c)
                 , cube_z = axial_r c
                 }

cube2offset :: Offset -> CubeCoordinate -> OffsetCoordinate
cube2offset offset c =
  let orientation = cube_orientation c
      op = case offset of
             Even -> (+)
             Odd -> (-)
      (q, r) = case orientation of
                Vertical -> (cube_x c, offsetF (cube_z c) (cube_x c) op)
                Horizontal -> (offsetF (cube_x c) (cube_z c) op , cube_z c)

  in OffsetCoordinate { offset_orientation = orientation
                      , offset_offset = offset
                      , offset_q = q
                      , offset_r = r
                      }
  where offsetF :: Int -> Int -> (Double -> Double -> Double) -> Int
        offsetF a b = offsetHelper a b (+)

offset2cube :: OffsetCoordinate -> CubeCoordinate
offset2cube c =
  let orientation = offset_orientation c
      op = case (offset_offset c) of
             Even -> (+)
             Odd -> (-)
      (x,z) = case orientation of
               Vertical -> (offset_q c, offsetF (offset_r c) (offset_q c) op)
               Horizontal -> (offsetF (offset_q c) (offset_r c) op, offset_r c)
      y = -x-z
  in CubeCoordinate { cube_orientation = orientation
                    , cube_x = x
                    , cube_y = y
                    , cube_z = z
                    }
  where offsetF a b = offsetHelper a b (-)

offsetHelper :: Int -> Int -> (Double -> Double -> Double) -> (Double -> Double -> Double) -> Int
offsetHelper a b op1 op2 =
  let a' = fromIntegral a
      b' = fromIntegral b
  in round $ a' `op1` (b' `op2` (fromIntegral $ b .&. 1)) / 2


hexDim :: Hexgrid -> ScreenCoordinate
hexDim g = ScreenCoordinate (round $ hexWidth g) (round $ hexHeight g)

hexDist :: Hexgrid -> ScreenCoordinate
hexDist g = ScreenCoordinate (round $ hexHorizontalDist g) (round $ hexVerticalDist g)


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



{-
tg :: Hexgrid
tg = Hexgrid {
  grid_orientation = Horizontal,
  grid_hex_size = 80,
  grid_width = 14,
  grid_height = 14,
  grid_offset = Even
  }


t g = [ AxialCoordinate x y | x <- [0 .. (grid_width g)], y <- [0 .. (grid_height g)]   ]
toff g = map (cubeHexagonOffset g . axial2cube) $ t g


a = (qq $ t tg,  rr $ t tg)
b = (qq $ toff tg,  rr $ toff tg)

rr cs = (minq cs, maxq cs)
qq cs = (minr cs, maxr cs)

maxq (c:cs) = foldl max (axial_q c) $ map axial_q $ cs
minq (c:cs) = foldl min (axial_q c) $ map axial_q $ cs
maxr (c:cs) = foldl max (axial_r c) $ map axial_r $ cs
minr (c:cs) = foldl min (axial_r c) $ map axial_r $ cs

-}



#ifdef FAY
(.&.) :: Int -> Int -> Int
(.&.) = ffi "(%1 & %2)"

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
