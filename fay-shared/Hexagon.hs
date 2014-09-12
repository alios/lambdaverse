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

class HexCoordinate c where
  neighbors :: c -> [c]
  diagonals :: c -> [c]
  distance :: c -> c -> Double
  line :: c -> c -> [c]
  range :: Int -> c -> [c]

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
                  , cube_x           :: Double
                  , cube_y           :: Double
                  , cube_z           :: Double
} deriving (Show, Read, Eq, Typeable, Data)

data Orientation = Horizontal | Vertical
    deriving (Show, Read, Eq, Typeable, Data)
data Offset = Odd | Even
    deriving (Show, Read, Eq, Typeable, Data)

cube2axial :: CubeCoordinate -> AxialCoordinate
cube2axial c' =
  let c = hexRound c'
  in AxialCoordinate { axial_orientation = cube_orientation c
                     , axial_q = round $ cube_x c
                     , axial_r = round $ cube_z c
                     }

axial2cube :: AxialCoordinate -> CubeCoordinate
axial2cube c =
  CubeCoordinate { cube_orientation = axial_orientation c
                 , cube_x = fromIntegral $ axial_q c
                 , cube_y = fromIntegral $ -(axial_q c) - (axial_r c)
                 , cube_z = fromIntegral $ axial_r c
                 }

cube2offset :: Offset -> CubeCoordinate -> OffsetCoordinate
cube2offset offset c' =
  let c = hexRound c'
      orientation = cube_orientation c
      op = case offset of
             Even -> (+)
             Odd -> (-)
      (q, r) = case orientation of
                Vertical -> (cube_x c, offsetF (cube_z c) (cube_x c) op)
                Horizontal -> (offsetF (cube_x c) (cube_z c) op , cube_z c)

  in OffsetCoordinate { offset_orientation = orientation
                      , offset_offset = offset
                      , offset_q = round $ q
                      , offset_r = round $ r
                      }
  where offsetF a b = offsetHelper a b (+)

offset2cube :: OffsetCoordinate -> CubeCoordinate
offset2cube c =
  let orientation = offset_orientation c
      op = case (offset_offset c) of
             Even -> (+)
             Odd -> (-)
      (x,z) = case orientation of
               Vertical -> (fromIntegral $ offset_q c
                           , offsetF (fromIntegral $ offset_r c) (fromIntegral $ offset_q c) op)
               Horizontal -> (offsetF (fromIntegral $ offset_q c) (fromIntegral $ offset_r c) op
                             ,fromIntegral $ offset_r c)
      y = -x-z
  in CubeCoordinate { cube_orientation = orientation
                    , cube_x = x
                    , cube_y = y
                    , cube_z = z
                    }
  where offsetF a b = offsetHelper a b (-)

offsetHelper  :: Double -> Double -> (Double -> Double -> Double) -> (Double -> Double -> Double) -> Double
offsetHelper a b op1 op2 =
  let c = if (oddD b) then 1 else 0
      oddD :: Double -> Bool
      oddD d = odd $ ((round d) :: Int)
  in a `op1` (b `op2` c)



axial2offset :: Offset -> AxialCoordinate -> OffsetCoordinate
axial2offset o = cube2offset o .  axial2cube

offset2axial :: OffsetCoordinate -> AxialCoordinate
offset2axial = cube2axial . offset2cube

cubeNeighbors :: CubeCoordinate -> [CubeCoordinate]
cubeNeighbors c =
  let ms = [(1,-1,0),(1,0,-1),(0,1,-1)
           ,(-1,1,0),(-1,0,1), (0,-1,1)]
  in [ c { cube_x = cube_x c + dx
         , cube_y = cube_y c + dy
         , cube_z = cube_z c + dz
         } | (dx,dy,dz) <- ms]

axialNeighbors :: AxialCoordinate -> [AxialCoordinate]
axialNeighbors c =
  let ms = [(1,0),(1,-1),(0,-1)
           ,(-1,0),(-1,1),(0,1)]
  in [ c { axial_q = axial_q c + dq
         , axial_r = axial_r c + dr
         } | (dq,dr) <- ms ]

offsetNeighbors :: OffsetCoordinate -> [OffsetCoordinate]
offsetNeighbors = mapOffsetOverCube cubeNeighbors

cubeDiagonals :: CubeCoordinate -> [CubeCoordinate]
cubeDiagonals c =
  let  ms = [(2,-1,-1),(1,1,-2),(-1,2,-1)
            ,(-2,1,1),(-1,-1,2),(1,-2,1)]
  in [ c { cube_x = cube_x c + dx
         , cube_y = cube_y c + dy
         , cube_z = cube_z c + dz
         } | (dx,dy,dz) <- ms ]

offsetDiagonals :: OffsetCoordinate -> [OffsetCoordinate]
offsetDiagonals = mapOffsetOverCube cubeDiagonals

axialDiagonals :: AxialCoordinate -> [AxialCoordinate]
axialDiagonals = mapAxialOverCube cubeDiagonals

cubeDistance :: CubeCoordinate -> CubeCoordinate -> Double
cubeDistance c1 c2 =
  maximum [ abs $ (cube_x c1) - (cube_x c2)
          , abs $ (cube_y c1) - (cube_y c2)
          , abs $ (cube_z c1) - (cube_z c2)
          ]

axialDistance :: AxialCoordinate -> AxialCoordinate -> Int
axialDistance c1 c2 = round $ cubeDistance (axial2cube c1) (axial2cube c2)

offsetDistance :: OffsetCoordinate -> OffsetCoordinate -> Int
offsetDistance c1 c2 = round $ cubeDistance (offset2cube c1) (offset2cube c2)

mapOffsetOverCube :: (CubeCoordinate -> [CubeCoordinate]) -> OffsetCoordinate -> [OffsetCoordinate]
mapOffsetOverCube f c = map (cube2offset (offset_offset c)) $ f (offset2cube c)

mapAxialOverCube :: (CubeCoordinate -> [CubeCoordinate]) -> AxialCoordinate -> [AxialCoordinate]
mapAxialOverCube f c = map (cube2axial) $ f (axial2cube c)


hexRound :: CubeCoordinate -> CubeCoordinate
hexRound c =
  let  rx = fromIntegral $ (round x :: Int)
       ry = fromIntegral $ (round y :: Int)
       rz = fromIntegral $ (round z :: Int)
       x = cube_x c
       y = cube_y c
       z = cube_z c
       x_diff = abs (rx - x)
       y_diff = abs (ry - y)
       z_diff = abs (rz - z)
       (x', y', z') =
         if (x_diff > y_diff && x_diff > z_diff)
            then (-ry-rz, ry, rz)
            else if (y_diff > z_diff)
                    then (rx, -rx-rz, rz)
                    else (rx, ry, -rx-ry)
  in c { cube_x = x', cube_y = y', cube_z = z' }

multCube :: CubeCoordinate -> Double -> CubeCoordinate
multCube c d = c { cube_x = cube_x c * d
                 , cube_y = cube_y c * d
                 , cube_z = cube_z c * d
                 }

plusCube :: CubeCoordinate -> CubeCoordinate -> CubeCoordinate
plusCube a b
  | (cube_orientation a /= cube_orientation b) = error "plusCube: coordinates must have same orientation"
  | otherwise = a { cube_x = cube_x a + cube_x b
                  , cube_y = cube_y a + cube_y b
                  , cube_z = cube_z a + cube_z b
                  }
cubeLine :: CubeCoordinate -> CubeCoordinate -> [CubeCoordinate]
cubeLine a b =
  let n = (floor $ distance a b)
      n :: Int
      f i = a `multCube` (1 - (i / fromIntegral n))
      g i = b `multCube` (i / fromIntegral n)
  in [ hexRound $ (f $ fromIntegral i) `plusCube`  (g $ fromIntegral i) | i <- [0..n]]

axialLine :: AxialCoordinate -> AxialCoordinate -> [AxialCoordinate]
axialLine a = mapAxialOverCube (cubeLine (axial2cube a))

offsetLine :: OffsetCoordinate -> OffsetCoordinate -> [OffsetCoordinate]
offsetLine a = mapOffsetOverCube (cubeLine (offset2cube a))

cubeRange :: Int -> CubeCoordinate -> [CubeCoordinate]
cubeRange n c =
  let xs = [-n .. n]
      ys x = [(max (-n) (-x-n)) .. (min n (-x+n))]
      zs x y = -x-y
  in [ c { cube_x = cube_x c + (fromIntegral x)
         , cube_y = cube_y c + (fromIntegral y)
         , cube_z = cube_z c + (fromIntegral $ zs x y)
         } | x <- xs, y <- ys x]

axialRange :: Int -> AxialCoordinate -> [AxialCoordinate]
axialRange n = mapAxialOverCube (cubeRange n)

offsetRange :: Int -> OffsetCoordinate -> [OffsetCoordinate]
offsetRange n = mapOffsetOverCube (cubeRange n)

screen2axial :: Hexgrid -> ScreenCoordinate -> AxialCoordinate
screen2axial g s =
  let  x = ((fromIntegral $ pixel_x s) - (hw / 2.0)) / hw
       hw = hexWidth g
       z = fromIntegral $ pixel_y s
--       z = fromIntegral $ - x - pixel_y
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
      q = fromIntegral $ offset_q c
      r = fromIntegral $ offset_r c
      op = if (offset_offset c == Odd) then odd else even
  in case (offset_orientation c) of
      Horizontal ->
        ScreenCoordinate { pixel_y = round $ hh * r
                         , pixel_x = round $ (hw * q) +
                                     (if (op $ offset_r c) then 0 else (hexWidth g) / 2)
                         }
      Vertical ->
        ScreenCoordinate { pixel_x = round $ hw * q
                         , pixel_y = round $ (hh * r) +
                                     (if (op $ offset_q c) then 0 else (hexHeight g) / 2)
                         }

axial2screen :: Hexgrid -> Offset -> AxialCoordinate -> ScreenCoordinate
axial2screen g o = offset2screen g . axial2offset o

cube2screen :: Hexgrid -> Offset -> CubeCoordinate -> ScreenCoordinate
cube2screen g o = offset2screen g . cube2offset o

data ScreenCoordinate =
  ScreenCoordinate { pixel_x :: Int
                   , pixel_y :: Int
} deriving (Show, Read, Eq, Typeable, Data)

data ScreenBox =
  ScreenBox { viewbox_pos  :: ScreenCoordinate
            , viewbox_size :: ScreenCoordinate
            } deriving (Show, Read, Eq, Typeable, Data)

mkScreenBox :: Int -> Int -> Int -> Int -> ScreenBox
mkScreenBox x y w h =
  ScreenBox { viewbox_pos  = ScreenCoordinate { pixel_x = x, pixel_y = y }
            , viewbox_size = ScreenCoordinate { pixel_x = w, pixel_y = h }
            }

inGrid :: Hexgrid -> OffsetCoordinate -> Bool
inGrid g c =
  let  q = offset_q c
       r = offset_r c
  in (q >= 0) && (q < grid_width g) && (r >= 0) && (r < grid_height g)

hexesFromScreenBox :: Hexgrid -> Offset -> ScreenBox -> [OffsetCoordinate]
hexesFromScreenBox g offset box =
  let pos1 = viewbox_pos box
      pos2 = pos1 `plusScreen` viewbox_size box
      a = screen2offset g offset pos1
      b = screen2offset g offset pos2
      qmin = min (offset_q a) (offset_q b)
      qmax = max (offset_q a) (offset_q b)
      rmin = min (offset_r a) (offset_r b)
      rmax = max (offset_r a) (offset_r b)
      qs = [qmin..qmax]
      rs = [rmin..rmax]
  in filter (inGrid g) [
    OffsetCoordinate { offset_offset = offset
                     , offset_orientation = grid_orientation g
                     , offset_q = q
                     , offset_r = r
                     } | r <- rs , q <- qs ]




multScreen :: Double -> ScreenCoordinate -> ScreenCoordinate
multScreen d s =
  let vmul a = round $ d * (fromIntegral a)
  in ScreenCoordinate { pixel_x = vmul $ pixel_x s
                      , pixel_y = vmul $ pixel_y s
                      }

plusScreen :: ScreenCoordinate -> ScreenCoordinate -> ScreenCoordinate
plusScreen a b
  = ScreenCoordinate { pixel_x = pixel_x a + pixel_x b
                     , pixel_y = pixel_y a + pixel_y b
                     }









instance HexCoordinate CubeCoordinate where
  neighbors = cubeNeighbors
  diagonals = cubeDiagonals
  distance = cubeDistance
  line = cubeLine
  range = cubeRange

instance HexCoordinate OffsetCoordinate where
  neighbors = offsetNeighbors
  diagonals = offsetDiagonals
  distance a b = fromIntegral $ offsetDistance a b
  line = offsetLine
  range = offsetRange

instance HexCoordinate AxialCoordinate where
  neighbors = axialNeighbors
  diagonals = axialDiagonals
  distance a b = fromIntegral $ axialDistance a b
  line = axialLine
  range = axialRange

--
-- The Hexgrid
--

data Hexgrid =
  Hexgrid {
    grid_orientation :: Orientation,
    grid_hex_size    :: Double,
    grid_width       :: Int,
    grid_height      :: Int
    }  deriving (Show, Read, Eq, Typeable, Data)


mkGridCoordinate :: Hexgrid -> Int -> Int -> AxialCoordinate
mkGridCoordinate g q r =
  AxialCoordinate { axial_orientation = grid_orientation g
                  , axial_q = q
                  , axial_r = r
                  }



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


{-
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

-}

renderHexagon :: Paper -> Hexgrid -> String -> ScreenCoordinate -> Fay Element
renderHexagon paper g str center = do
  poly <- renderHexagonPolygon paper g center
  let q = axial_q a
      r = axial_r a
      x = pixel_x center
      y = pixel_y center
      a = screen2axial g center
  _ <- paperText paper x y str
  _ <- paperText paper x (y + 15) $ show x ++ "/" ++ show y
  return poly

renderHexagonPolygon :: Paper -> Hexgrid -> ScreenCoordinate -> Fay Element
renderHexagonPolygon paper g center =
  pathCommands ((map mkArgs [0..5]) ++ [ClosePath]) paper
  where mkArgs i =
          let size = grid_hex_size g
              center_x = fromIntegral $ pixel_x center
              center_y = fromIntegral $ pixel_y center
              angle_offset = if (grid_orientation g == Horizontal) then 0.5 else 0.0
              angle = 2 * pi / 6 * (i + angle_offset)
              x_i = round $ center_x + size * cos(angle)
              y_i = round $ center_y + size * sin(angle)
              args = (x_i, y_i)
          in if (i == 0) then MoveTo args else LineTo args

setPaperViewBox :: Paper -> ScreenBox -> Fay ()
setPaperViewBox paper vb =
  let x = pixel_x . viewbox_pos $ vb
      y = pixel_y . viewbox_pos $ vb
      w = pixel_x . viewbox_size $ vb
      h = pixel_y . viewbox_size $ vb
  in setViewBox paper x y w h False


#endif
