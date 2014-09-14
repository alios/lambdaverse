{-# LANGUAGE DeriveDataTypeable #-}

module Data.Hexagon.OffsetCoordinate where

import           Data.Data
import           Data.Hexagon.Classes
import           Data.Hexagon.CubeCoordinate
import           Data.Hexagon.Types
import           Prelude

data OffsetCoordinate =
  OffsetCoordinate  { _offset_orientation :: Orientation
                    , _offset_offset      :: Offset
                    , _offset_q           :: Int
                    , _offset_r           :: Int
} deriving (Show, Read, Eq, Typeable, Data)


instance Coordinate OffsetCoordinate where
  neighbors = offsetNeighbors
  diagonals = offsetDiagonals
  distance a b = fromIntegral $ offsetDistance a b
  line = offsetLine
  range = offsetRange


offsetNeighbors :: OffsetCoordinate -> [OffsetCoordinate]
offsetNeighbors = mapOffsetOverCube cubeNeighbors

offsetDiagonals :: OffsetCoordinate -> [OffsetCoordinate]
offsetDiagonals = mapOffsetOverCube cubeDiagonals

offsetDistance :: OffsetCoordinate -> OffsetCoordinate -> Int
offsetDistance c1 c2 = round $ cubeDistance (offset2cube c1) (offset2cube c2)

offsetLine :: OffsetCoordinate -> OffsetCoordinate -> [OffsetCoordinate]
offsetLine a = mapOffsetOverCube (cubeLine (offset2cube a))

offsetRange :: Int -> OffsetCoordinate -> [OffsetCoordinate]
offsetRange n = mapOffsetOverCube (cubeRange n)

cube2offset :: Offset -> CubeCoordinate -> OffsetCoordinate
cube2offset offset c' =
  let c = hexRound c'
      orientation = _cube_orientation c
      op = case offset of
             Even -> (+)
             Odd -> (-)
      (q, r) = case orientation of
                Vertical -> (_cube_x c, offsetF (_cube_z c) (_cube_x c) op)
                Horizontal -> (offsetF (_cube_x c) (_cube_z c) op , _cube_z c)

  in OffsetCoordinate { _offset_orientation = orientation
                      , _offset_offset = offset
                      , _offset_q = round $ q
                      , _offset_r = round $ r
                      }
  where offsetF a b = offsetHelper a b (+)

offset2cube :: OffsetCoordinate -> CubeCoordinate
offset2cube c =
  let orientation = _offset_orientation c
      op = case (_offset_offset c) of
             Even -> (+)
             Odd -> (-)
      (x,z) = case orientation of
               Vertical -> (fromIntegral $ _offset_q c
                           , offsetF (fromIntegral $ _offset_r c) (fromIntegral $ _offset_q c) op)
               Horizontal -> (offsetF (fromIntegral $ _offset_q c) (fromIntegral $ _offset_r c) op
                             ,fromIntegral $ _offset_r c)
      y = -x-z
  in CubeCoordinate { _cube_orientation = orientation
                    , _cube_x = x
                    , _cube_y = y
                    , _cube_z = z
                    }
  where offsetF a b = offsetHelper a b (-)


mapOffsetOverCube :: (CubeCoordinate -> [CubeCoordinate]) -> OffsetCoordinate -> [OffsetCoordinate]
mapOffsetOverCube f c = fmap (cube2offset (_offset_offset c)) $ f (offset2cube c)
