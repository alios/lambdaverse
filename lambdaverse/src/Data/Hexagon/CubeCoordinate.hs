{-# LANGUAGE DeriveDataTypeable #-}

module Data.Hexagon.CubeCoordinate where

import           Data.Data
import           Data.Hexagon.Classes
import           Data.Hexagon.Types
import           Prelude


data CubeCoordinate =
  CubeCoordinate  { _cube_orientation :: Orientation
                  , _cube_x           :: Double
                  , _cube_y           :: Double
                  , _cube_z           :: Double
} deriving (Show, Read, Eq, Typeable, Data)


instance Coordinate CubeCoordinate where
  neighbors = cubeNeighbors
  diagonals = cubeDiagonals
  distance = cubeDistance
  line = cubeLine
  range = cubeRange


cubeNeighbors :: CubeCoordinate -> [CubeCoordinate]
cubeNeighbors c =
  let ms = [(1,-1,0),(1,0,-1),(0,1,-1)
           ,(-1,1,0),(-1,0,1), (0,-1,1)]
  in [ c { _cube_x = _cube_x c + dx
         , _cube_y = _cube_y c + dy
         , _cube_z = _cube_z c + dz
         } | (dx,dy,dz) <- ms]


cubeDiagonals :: CubeCoordinate -> [CubeCoordinate]
cubeDiagonals c =
  let  ms = [(2,-1,-1),(1,1,-2),(-1,2,-1)
            ,(-2,1,1),(-1,-1,2),(1,-2,1)]
  in [ c { _cube_x = _cube_x c + dx
         , _cube_y = _cube_y c + dy
         , _cube_z = _cube_z c + dz
         } | (dx,dy,dz) <- ms ]



cubeDistance :: CubeCoordinate -> CubeCoordinate -> Double
cubeDistance c1 c2 =
  maximum [ abs $ (_cube_x c1) - (_cube_x c2)
          , abs $ (_cube_y c1) - (_cube_y c2)
          , abs $ (_cube_z c1) - (_cube_z c2)
          ]

hexRound :: CubeCoordinate -> CubeCoordinate
hexRound c =
  let  rx = fromIntegral $ (round x :: Int)
       ry = fromIntegral $ (round y :: Int)
       rz = fromIntegral $ (round z :: Int)
       x = _cube_x c
       y = _cube_y c
       z = _cube_z c
       x_diff = abs (rx - x)
       y_diff = abs (ry - y)
       z_diff = abs (rz - z)
       (x', y', z') =
         if (x_diff > y_diff && x_diff > z_diff)
            then (-ry-rz, ry, rz)
            else if (y_diff > z_diff)
                    then (rx, -rx-rz, rz)
                    else (rx, ry, -rx-ry)
  in c { _cube_x = x', _cube_y = y', _cube_z = z' }

multCube :: CubeCoordinate -> Double -> CubeCoordinate
multCube c d = c { _cube_x = _cube_x c * d
                 , _cube_y = _cube_y c * d
                 , _cube_z = _cube_z c * d
                 }

plusCube :: CubeCoordinate -> CubeCoordinate -> CubeCoordinate
plusCube a b
  | (_cube_orientation a /= _cube_orientation b) = error "plusCube: coordinates must have same orientation"
  | otherwise = a { _cube_x = _cube_x a + _cube_x b
                  , _cube_y = _cube_y a + _cube_y b
                  , _cube_z = _cube_z a + _cube_z b
                  }
cubeLine :: CubeCoordinate -> CubeCoordinate -> [CubeCoordinate]
cubeLine a b =
  let n = (floor $ distance a b)
      n :: Int
      f i = a `multCube` (1 - (i / fromIntegral n))
      g i = b `multCube` (i / fromIntegral n)
  in [ hexRound $ (f $ fromIntegral i) `plusCube`  (g $ fromIntegral i) | i <- [0..n]]


cubeRange :: Int -> CubeCoordinate -> [CubeCoordinate]
cubeRange n c =
  let xs = [-n .. n]
      ys x = [(max (-n) (-x-n)) .. (min n (-x+n))]
      zs x y = -x-y
  in [ c { _cube_x = _cube_x c + (fromIntegral x)
         , _cube_y = _cube_y c + (fromIntegral y)
         , _cube_z = _cube_z c + (fromIntegral $ zs x y)
         } | x <- xs, y <- ys x]

offsetHelper  :: Double -> Double -> (Double -> Double -> Double) -> (Double -> Double -> Double) -> Double
offsetHelper a b op1 op2 =
  let c = if (oddD b) then 1 else 0
      oddD :: Double -> Bool
      oddD d = odd $ ((round d) :: Int)
  in a `op1` (b `op2` c)
