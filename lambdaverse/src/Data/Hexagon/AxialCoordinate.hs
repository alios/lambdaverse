{-# LANGUAGE DeriveDataTypeable #-}

module Data.Hexagon.AxialCoordinate where

import           Data.Data
import           Data.Hexagon.Classes
import           Data.Hexagon.CubeCoordinate
import           Data.Hexagon.Types
import           Prelude

data AxialCoordinate =
  AxialCoordinate  { _axial_orientation :: Orientation
                   , _axial_q           :: Int
                   , _axial_r           :: Int
} deriving (Show, Read, Eq, Typeable, Data)


instance Coordinate AxialCoordinate where
  neighbors = axialNeighbors
  diagonals = axialDiagonals
  distance a b = fromIntegral $ axialDistance a b
  line = axialLine
  range = axialRange



axialNeighbors :: AxialCoordinate -> [AxialCoordinate]
axialNeighbors c =
  let ms = [(1,0),(1,-1),(0,-1)
           ,(-1,0),(-1,1),(0,1)]
  in [ c { _axial_q = _axial_q c + dq
         , _axial_r = _axial_r c + dr
         } | (dq,dr) <- ms ]

axialDiagonals :: AxialCoordinate -> [AxialCoordinate]
axialDiagonals = mapAxialOverCube cubeDiagonals

axialDistance :: AxialCoordinate -> AxialCoordinate -> Int
axialDistance c1 c2 = round $ cubeDistance (axial2cube c1) (axial2cube c2)

axialLine :: AxialCoordinate -> AxialCoordinate -> [AxialCoordinate]
axialLine a = mapAxialOverCube (cubeLine (axial2cube a))

axialRange :: Int -> AxialCoordinate -> [AxialCoordinate]
axialRange n = mapAxialOverCube (cubeRange n)

cube2axial :: CubeCoordinate -> AxialCoordinate
cube2axial c' =
  let c = hexRound c'
  in AxialCoordinate { _axial_orientation = _cube_orientation c
                     , _axial_q = round $ _cube_x c
                     , _axial_r = round $ _cube_z c
                     }

axial2cube :: AxialCoordinate -> CubeCoordinate
axial2cube c =
  CubeCoordinate { _cube_orientation = _axial_orientation c
                 , _cube_x = fromIntegral $ _axial_q c
                 , _cube_y = fromIntegral $ -(_axial_q c) - (_axial_r c)
                 , _cube_z = fromIntegral $ _axial_r c
                 }

mapAxialOverCube :: (CubeCoordinate -> [CubeCoordinate]) -> AxialCoordinate -> [AxialCoordinate]
mapAxialOverCube f c = fmap (cube2axial) $ f (axial2cube c)
