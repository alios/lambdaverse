{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HexagonTests where

import           Hexagon
import           Hexagon.Test
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
    q <- arbitrary
    r <- arbitrary
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


posInt :: Gen Int
posInt = arbitrary `suchThat` (>= 0)



hexagonSpec :: Spec
hexagonSpec =
  describe "testing Hexagon" $ do
    conversionSpec
    neighborsSpec
    distanceSpec
    diagonalsSpec
    rangeSpec

testHexagon  :: IO ()
testHexagon = hspec hexagonSpec




conversionSpec :: Spec
conversionSpec =
  describe "testing hexagon coordinate conversions" $ do
    it "convert from cube to axial and back" $ property prop_c2a
    it "convert from axial to cube and back" $ property prop_a2c
    it "convert from cube to offset and back" $ property prop_c2o
    it "convert from offset to cube and back" $ property prop_o2c
    it "convert from axial to offset and back" $ property prop_a2o
    it "convert from offset to axial and back" $ property prop_o2a


neighborsSpec :: Spec
neighborsSpec =
  describe "testing neighbors by" $ do
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

rangeSpec :: Spec
rangeSpec = do
  describe "range calc produces cube coordinates" $
    it "which are valid" $ property prop_range_valid
  describe "range returns (3n^2 - 3n + 1) | n' = n-1 coordinates" $ do
    it "with CubeCoordinate" $ property prop_range_lengthC
    it "with AxialCoordinate" $ property prop_range_lengthA
    it "with OffsetCoordinate" $ property prop_range_lengthO
  describe "range 1 equals neighbors c + c" $ do
    it "with CubeCoordinate" $ property prop_range_neighborsC
    it "with AxialCoordinate" $ property prop_range_neighborsA
    it "with OffsetCoordinate" $ property prop_range_neighborsO

prop_range_lengthC :: Property
prop_range_lengthC = forAll posInt  prop_range_lengthC'
prop_range_lengthA :: Property
prop_range_lengthA = forAll posInt  prop_range_lengthA'
prop_range_lengthO :: Property
prop_range_lengthO = forAll posInt  prop_range_lengthO'


