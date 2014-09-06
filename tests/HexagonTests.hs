{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HexagonTests where

import           Hexagon
import           Prelude
import           Test.Hspec
import           Test.QuickCheck

instance Arbitrary Orientation where
  arbitrary = elements [ Horizontal, Vertical ]

instance Arbitrary Offset where
  arbitrary = elements [ Even, Odd ]

instance Arbitrary OffsetCoordinate where
  arbitrary = do
    (offset, orientation) <- arbitrary
    q <- arbitrary -- `suchThat` (>= 0)
    r <- arbitrary -- `suchThat` (>= 0)
    return $
      OffsetCoordinate { offset_offset = offset
                       , offset_orientation = orientation
                       , offset_q = q
                       , offset_r = r
                       }

instance Arbitrary AxialCoordinate where
  arbitrary = do
    (q,r,orientation) <- arbitrary
    return $
      AxialCoordinate { axial_orientation = orientation
                      , axial_q = q
                      , axial_r = r
                      }

instance Arbitrary CubeCoordinate where
  arbitrary = do
    (x,y, orientation) <- arbitrary :: Gen (Int, Int, Orientation)
    let z = (-x) - y
    return $
      CubeCoordinate { cube_orientation = orientation
                     , cube_x = fromIntegral x
                     , cube_y = fromIntegral y
                     , cube_z = fromIntegral z
                     }


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


conversionSpec :: Spec
conversionSpec =
  describe "testing hexagon coordinate conversions" $ do
    it "convert from cube to axial and back" $ property prop_c2a
    it "convert from axial to cube and back" $ property prop_a2c
    it "convert from cube to offset and back" $ property prop_c2o
    it "convert from offset to cube and back" $ property prop_o2c
    it "convert from axial to offset and back" $ property prop_a2o
    it "convert from offset to axial and back" $ property prop_o2a

prop_neighbors_cube_axial :: CubeCoordinate -> Bool
prop_neighbors_cube_axial c =
  let cs = cubeNeighbors c
      a = cube2axial c
      as = axialNeighbors a
      cas = fmap axial2cube as
  in cas `eqList` cs
prop_neighbors_cube_offset :: Offset -> CubeCoordinate -> Bool
prop_neighbors_cube_offset o c =
  let cs = cubeNeighbors c
      a = cube2offset o c
      as = offsetNeighbors a
      cas = fmap offset2cube as
  in cas `eqList` cs

prop_neighbors_axial_cube :: AxialCoordinate -> Bool
prop_neighbors_axial_cube c =
  let cs = axialNeighbors c
      a = axial2cube c
      as = cubeNeighbors a
      cas = fmap cube2axial as
  in cas `eqList` cs

prop_neighbors_axial_offset :: Offset -> AxialCoordinate -> Bool
prop_neighbors_axial_offset o c =
  let cs = axialNeighbors c
      a = axial2offset o c
      as = offsetNeighbors a
      cas = fmap offset2axial as
  in cas `eqList` cs

prop_neighbors_offset_axial :: OffsetCoordinate -> Bool
prop_neighbors_offset_axial c =
  let cs = offsetNeighbors c
      a = offset2axial c
      as = axialNeighbors a
      o = offset_offset c
      cas = fmap (axial2offset o) as
  in cas `eqList` cs

prop_neighbors_offset_cube :: OffsetCoordinate -> Bool
prop_neighbors_offset_cube c =
  let cs = offsetNeighbors c
      a = offset2cube c
      as = cubeNeighbors a
      o = offset_offset c
      cas = fmap (cube2offset o) as
  in cas `eqList` cs


neighborsSpec :: Spec
neighborsSpec =
  describe "testing distance by" $ do
    it "comapring cubeNeighbors to axialNeighbors from cube" $
       property prop_neighbors_cube_axial
    it "comapring cubeNeighbors to offsetNeighbors from cube" $
       property prop_neighbors_cube_offset
    it "comapring axialNeighbors to cubeNeighbors from axial" $
       property prop_neighbors_axial_cube
    it "comapring axialNeighbors to offsetNeighbors from axial" $
       property prop_neighbors_axial_offset
    it "comapring offsetNeighbors to axialNeighbors from offset" $
       property prop_neighbors_offset_axial
    it "comapring offsetNeighbors to cubeNeighbors from offset" $
       property prop_neighbors_offset_cube



prop_diagonals_cube_axial :: CubeCoordinate -> Bool
prop_diagonals_cube_axial c =
  let cs = cubeDiagonals c
      a = cube2axial c
      as = axialDiagonals a
      cas = fmap axial2cube as
  in cas `eqList` cs
prop_diagonals_cube_offset :: Offset -> CubeCoordinate -> Bool
prop_diagonals_cube_offset o c =
  let cs = cubeDiagonals c
      a = cube2offset o c
      as = offsetDiagonals a
      cas = fmap offset2cube as
  in cas `eqList` cs

prop_diagonals_axial_cube :: AxialCoordinate -> Bool
prop_diagonals_axial_cube c =
  let cs = axialDiagonals c
      a = axial2cube c
      as = cubeDiagonals a
      cas = fmap cube2axial as
  in cas `eqList` cs

prop_diagonals_axial_offset :: Offset -> AxialCoordinate -> Bool
prop_diagonals_axial_offset o c =
  let cs = axialDiagonals c
      a = axial2offset o c
      as = offsetDiagonals a
      cas = fmap offset2axial as
  in cas `eqList` cs

prop_diagonals_offset_axial :: OffsetCoordinate -> Bool
prop_diagonals_offset_axial c =
  let cs = offsetDiagonals c
      a = offset2axial c
      as = axialDiagonals a
      o = offset_offset c
      cas = fmap (axial2offset o) as
  in cas `eqList` cs

prop_diagonals_offset_cube :: OffsetCoordinate -> Bool
prop_diagonals_offset_cube c =
  let cs = offsetDiagonals c
      a = offset2cube c
      as = cubeDiagonals a
      o = offset_offset c
      cas = fmap (cube2offset o) as
  in cas `eqList` cs


diagonalsSpec :: Spec
diagonalsSpec =
  describe "testing diagonals by" $ do
    it "comapring cubeDiagonals to axialDiagonals from cube" $
       property prop_diagonals_cube_axial
    it "comapring cubeDiagonals to offsetDiagonals from cube" $
       property prop_diagonals_cube_offset
    it "comapring axialDiagonals to cubeDiagonals from axial" $
       property prop_diagonals_axial_cube
    it "comapring axialDiagonals to offsetDiagonals from axial" $
       property prop_diagonals_axial_offset
    it "comapring offsetDiagonals to axialDiagonals from offset" $
       property prop_diagonals_offset_axial
    it "comapring offsetDiagonals to cubeDiagonals from offset" $
       property prop_diagonals_offset_cube

prop_distance_axial_cube :: AxialCoordinate -> AxialCoordinate -> Bool
prop_distance_axial_cube a b =
  let cd = distance (axial2cube a) (axial2cube b)
  in cd == distance a b

prop_distance_axial_offset :: Offset -> AxialCoordinate -> AxialCoordinate -> Bool
prop_distance_axial_offset o a b =
  let cd = distance (axial2offset o a) (axial2offset o b)
  in cd == distance a b

prop_distance_cube_axial :: CubeCoordinate -> CubeCoordinate -> Bool
prop_distance_cube_axial a b =
  let cd = distance (cube2axial a) (cube2axial b)
  in cd == distance a b
prop_distance_cube_offset :: Offset -> CubeCoordinate -> CubeCoordinate -> Bool
prop_distance_cube_offset o a b =
  let cd = distance (cube2offset o a) (cube2offset o b)
  in cd == distance a b

prop_distance_offset_axial :: OffsetCoordinate -> OffsetCoordinate -> Bool
prop_distance_offset_axial a b =
  let cd = distance (offset2axial a) (offset2axial b)
  in cd == distance a b

prop_distance_offset_cube :: OffsetCoordinate -> OffsetCoordinate -> Bool
prop_distance_offset_cube a b =
  let cd = distance (offset2cube a) (offset2cube b)
  in cd == distance a b



distanceSpec :: Spec
distanceSpec =
  describe "testing neighbors by" $ do
    it "comapring cubeDistance to axialDistance from cube" $
       property prop_distance_cube_axial
    it "comapring cubeDistance to offsetDistance from cube" $
       property prop_distance_cube_offset
    it "comapring axialDistance to cubeDistance from axial" $
       property prop_distance_axial_cube
    it "comapring axialDistance to offsetDistance from axial" $
       property prop_distance_axial_offset
    it "comapring offsetDistance to axialDistance from offset" $
       property prop_distance_offset_axial
    it "comapring offsetDistance to cubeDistance from offset" $
       property prop_distance_offset_cube



hexagonSpec :: Spec
hexagonSpec =
  describe "testing Hexagon" $ do
    conversionSpec
    neighborsSpec
    distanceSpec
    diagonalsSpec

testHexagon  :: IO ()
testHexagon = hspec hexagonSpec

eqList :: Eq a => [a] -> [a] -> Bool
eqList cs ds = (length cs == length ds) && (and [elem c ds | c <- cs ])



