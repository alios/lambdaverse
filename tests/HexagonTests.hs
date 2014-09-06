{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HexagonTests where

import           Hexagon
import           Prelude
import           Test.QuickCheck
import           Test.Hspec

instance Arbitrary Orientation where
  arbitrary = elements [ Horizontal, Vertical ]

instance Arbitrary Offset where
  arbitrary = elements [ Even, Odd ]

instance Arbitrary OffsetCoordinate where
  arbitrary = do
    (offset, orientation) <- arbitrary
    q <- arbitrary `suchThat` (>= 0)
    r <- arbitrary `suchThat` (>= 0)
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
    (x,y,orientation) <- arbitrary
    let z = (-x) - y
    return $
      CubeCoordinate { cube_orientation = orientation
                     , cube_x = x
                     , cube_y = y
                     , cube_z = z
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

hexagonSpec :: Spec
hexagonSpec =
  describe "testing Hexagon" $ do
    conversionSpec


return []
runHexagonTests = $quickCheckAll
runHexagonTests :: IO Bool

