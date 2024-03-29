module Hexagon.Test where

import           Hexagon
import           Prelude

validCubeCoordinate :: CubeCoordinate -> Bool
validCubeCoordinate c = (cube_x c) + (cube_y c) + (cube_z c) == 0

prop_c2a :: CubeCoordinate -> Bool
prop_c2a c =
  let r = (axial2cube . cube2axial $ c)
  in (validCubeCoordinate c) && (validCubeCoordinate r) && (c == r)

prop_a2c :: AxialCoordinate -> Bool
prop_a2c c =
  let r = axial2cube $ c
  in (validCubeCoordinate r) && ((cube2axial r ) == c)

prop_c2o :: Offset -> CubeCoordinate -> Bool
prop_c2o o c =
  let r = (offset2cube . cube2offset o $ c)
  in (validCubeCoordinate c) && (validCubeCoordinate r) &&  (r == c)

prop_o2c :: OffsetCoordinate -> Bool
prop_o2c c =
  let o = offset_offset c
      r = offset2cube c
  in (validCubeCoordinate r) && ((cube2offset o r) == c)

prop_a2o :: Offset -> AxialCoordinate -> Bool
prop_a2o o c = (offset2axial . axial2offset o $ c) == c

prop_o2a :: OffsetCoordinate -> Bool
prop_o2a c = let o = offset_offset c
             in (axial2offset o . offset2axial $ c) == c


prop_neighbors_cube_axial :: CubeCoordinate -> Bool
prop_neighbors_cube_axial c =
  let cs = cubeNeighbors c
      a = cube2axial c
      as = axialNeighbors a
      cas = map axial2cube as
  in cas `eqList` cs
prop_neighbors_cube_offset :: Offset -> CubeCoordinate -> Bool
prop_neighbors_cube_offset o c =
  let cs = cubeNeighbors c
      a = cube2offset o c
      as = offsetNeighbors a
      cas = map offset2cube as
  in cas `eqList` cs

prop_neighbors_axial_cube :: AxialCoordinate -> Bool
prop_neighbors_axial_cube c =
  let cs = axialNeighbors c
      a = axial2cube c
      as = cubeNeighbors a
      cas = map cube2axial as
  in cas `eqList` cs

prop_neighbors_axial_offset :: Offset -> AxialCoordinate -> Bool
prop_neighbors_axial_offset o c =
  let cs = axialNeighbors c
      a = axial2offset o c
      as = offsetNeighbors a
      cas = map offset2axial as
  in cas `eqList` cs

prop_neighbors_offset_axial :: OffsetCoordinate -> Bool
prop_neighbors_offset_axial c =
  let cs = offsetNeighbors c
      a = offset2axial c
      as = axialNeighbors a
      o = offset_offset c
      cas = map (axial2offset o) as
  in cas `eqList` cs

prop_neighbors_offset_cube :: OffsetCoordinate -> Bool
prop_neighbors_offset_cube c =
  let cs = offsetNeighbors c
      a = offset2cube c
      as = cubeNeighbors a
      o = offset_offset c
      cas = map (cube2offset o) as
  in cas `eqList` cs


prop_diagonals_cube_axial :: CubeCoordinate -> Bool
prop_diagonals_cube_axial c =
  let cs = cubeDiagonals c
      a = cube2axial c
      as = axialDiagonals a
      cas = map axial2cube as
  in cas `eqList` cs
prop_diagonals_cube_offset :: Offset -> CubeCoordinate -> Bool
prop_diagonals_cube_offset o c =
  let cs = cubeDiagonals c
      a = cube2offset o c
      as = offsetDiagonals a
      cas = map offset2cube as
  in cas `eqList` cs

prop_diagonals_axial_cube :: AxialCoordinate -> Bool
prop_diagonals_axial_cube c =
  let cs = axialDiagonals c
      a = axial2cube c
      as = cubeDiagonals a
      cas = map cube2axial as
  in cas `eqList` cs

prop_diagonals_axial_offset :: Offset -> AxialCoordinate -> Bool
prop_diagonals_axial_offset o c =
  let cs = axialDiagonals c
      a = axial2offset o c
      as = offsetDiagonals a
      cas = map offset2axial as
  in cas `eqList` cs

prop_diagonals_offset_axial :: OffsetCoordinate -> Bool
prop_diagonals_offset_axial c =
  let cs = offsetDiagonals c
      a = offset2axial c
      as = axialDiagonals a
      o = offset_offset c
      cas = map (axial2offset o) as
  in cas `eqList` cs

prop_diagonals_offset_cube :: OffsetCoordinate -> Bool
prop_diagonals_offset_cube c =
  let cs = offsetDiagonals c
      a = offset2cube c
      as = cubeDiagonals a
      o = offset_offset c
      cas = map (cube2offset o) as
  in cas `eqList` cs


prop_distance_axial_cube :: AxialCoordinate -> AxialCoordinate -> Bool
prop_distance_axial_cube a b =
  let cd = cubeDistance (axial2cube a) (axial2cube b)
  in cd == (fromIntegral $ axialDistance a b)

prop_distance_axial_offset :: Offset -> AxialCoordinate -> AxialCoordinate -> Bool
prop_distance_axial_offset o a b =
  let cd = offsetDistance (axial2offset o a) (axial2offset o b)
  in (fromIntegral cd ) == axialDistance a b

prop_distance_cube_axial :: CubeCoordinate -> CubeCoordinate -> Bool
prop_distance_cube_axial a b =
  let cd = axialDistance (cube2axial a) (cube2axial b)
  in (fromIntegral cd) == cubeDistance a b

prop_distance_cube_offset :: Offset -> CubeCoordinate -> CubeCoordinate -> Bool
prop_distance_cube_offset o a b =
  let cd = fromIntegral $ offsetDistance (cube2offset o a) (cube2offset o b)
  in cd == cubeDistance a b

prop_distance_offset_axial :: OffsetCoordinate -> OffsetCoordinate -> Bool
prop_distance_offset_axial a b =
  let cd = fromIntegral $ axialDistance (offset2axial a) (offset2axial b)
  in cd == offsetDistance a b

prop_distance_offset_cube :: OffsetCoordinate -> OffsetCoordinate -> Bool
prop_distance_offset_cube a b =
  let cd = cubeDistance (offset2cube a) (offset2cube b)
  in cd == (fromIntegral $ offsetDistance a b)

prop_range_neighborsC :: CubeCoordinate -> Bool
prop_range_neighborsC c =
  (cubeRange 1 c) `eqList` (c : cubeNeighbors c)

prop_range_neighborsO :: OffsetCoordinate -> Bool
prop_range_neighborsO c =
  (offsetRange 1 c) `eqList` (c : offsetNeighbors c)

prop_range_neighborsA :: AxialCoordinate -> Bool
prop_range_neighborsA c =
  (axialRange 1 c) `eqList` (c : axialNeighbors c)

prop_range_lengthC' :: Int -> CubeCoordinate -> Bool
prop_range_lengthC' n c =
  let  l = (length $ cubeRange n c)
       n' = n + 1
  in l == ((3 * (n' ^ (2 ::Int) )) - (3*n') + 1)

prop_range_lengthA' :: Int -> AxialCoordinate -> Bool
prop_range_lengthA' n c =
  let  l = (length $ axialRange n c)
       n' = n + 1
  in l == ((3 * (n' ^ (2 ::Int) )) - (3*n') + 1)

prop_range_lengthO' :: Int -> OffsetCoordinate -> Bool
prop_range_lengthO' n c =
  let  l = (length $ offsetRange n c)
       n' = n + 1
  in l == ((3 * (n' ^ (2 ::Int) )) - (3*n') + 1)

prop_range_valid :: Int -> CubeCoordinate -> Bool
prop_range_valid n c = and $ map validCubeCoordinate $ cubeRange n c

eqList :: Eq a => [a] -> [a] -> Bool
eqList cs ds = (length cs == length ds) && (and [elem c ds | c <- cs ])


