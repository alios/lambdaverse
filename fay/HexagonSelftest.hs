module HexagonSelftest where

import           FFI
import           Hexagon
import           Hexagon.Test
import           Language.Fay.Yesod
import           Prelude
import           SharedTypes

data Event



data Assert
qunit_test :: String -> (Assert -> Fay ()) -> Fay ()
qunit_test = ffi "QUnit.test( %1, %2 )"

assert_ok :: Assert -> Bool -> String -> Fay ()
assert_ok = ffi "%1.ok( %2, %3 )"

addWindowEvent :: String -> (Event -> Fay ()) -> Fay ()
addWindowEvent = ffi "window.addEventListener(%1, %2)"

onLoad :: Event -> Fay ()
onLoad _ = do
  putStrLn "Initializing ..."
  conversionSpec
  neighborsSpec

testProp a desc p cs  =
  let ps = and $ map p cs
  in assert_ok a ps desc


conversionSpec :: Fay ()
conversionSpec = do
  cs <- sequence $ replicate 100 $ randomCubeCoordinate
  os <- sequence $ replicate 100 $ randomOffsetCoordinate
  as <- sequence $ replicate 100 $ randomAxialCoordinate
  qunit_test "testing hexagon coordinate conversions" $ \a -> do
    testProp a "convert from cube to axial and back" prop_c2a cs
    testProp a "convert from axial to cube and back" prop_a2c as
    testProp a "convert from offset to cube and back" prop_o2c os
    testProp a "convert from offset to axial and back" prop_o2a os
    testProp a "convert from cube to offset (odd) and back" (prop_c2o Odd) cs
    testProp a "convert from cube to offset (even) and back" (prop_c2o Even) cs
    testProp a "convert from axial to offset (odd) and back" (prop_a2o Odd) as
    testProp a "convert from axial to offset (even) and back" (prop_a2o Even) as

neighborsSpec :: Fay ()
neighborsSpec = do
  cs <- sequence $ replicate 100 $ randomCubeCoordinate
  os <- sequence $ replicate 100 $ randomOffsetCoordinate
  as <- sequence $ replicate 100 $ randomAxialCoordinate
  qunit_test "testing heighbors by" $ \a -> do
    testProp a "comapring cubeNeighbors to axialNeighbors from cube" prop_neighbors_cube_axial cs
    testProp a "comapring cubeNeighbors to offsetNeighbors (odd) from cube"
      (prop_neighbors_cube_offset Odd) cs
    testProp a "comapring cubeNeighbors to offsetNeighbors (even) from cube"
      (prop_neighbors_cube_offset Even) cs
    testProp a "comapring axialNeighbors to cubeNeighbors from axial" prop_neighbors_axial_cube as
    testProp a "comapring axialNeighbors to offsetNeighbors (odd) from axial"
      (prop_neighbors_axial_offset Odd) as
    testProp a "comapring axialNeighbors to offsetNeighbors (even) from axial"
      (prop_neighbors_axial_offset Even) as
    testProp a "comapring offsetNeighbors to axialNeighbors from offset" prop_neighbors_offset_axial os
    testProp a "comapring offsetNeighbors to cubeNeighbors from offset" prop_neighbors_offset_cube os





main :: Fay ()
main = do
  putStrLn "Hello Console!"
  addWindowEvent "load" onLoad
  return ()


randomD' :: Fay Double
randomD' = ffi "window.Math.random()"

randomD :: Double -> Fay Double
randomD n = do
  d <- randomD'
  return $ d * n

randomI :: Int -> Fay Int
randomI n = do
  d <- randomD'
  return . round $ d * fromIntegral n


randomOffset :: Fay Offset
randomOffset = do
  i <- randomI 1
  return $ if (i == 0) then Odd else Even

randomOrientation :: Fay Orientation
randomOrientation = do
  i <- randomI 1
  return $ if (i == 0) then Horizontal else Vertical


randomCubeCoordinate :: Fay CubeCoordinate
randomCubeCoordinate = do
  orientation <- randomOrientation
  x <- randomD 100
  y <- randomD 100
  return . hexRound $
    CubeCoordinate { cube_x = x
                   , cube_y = y
                   , cube_z = -x-y
                   , cube_orientation = orientation
                   }

randomAxialCoordinate :: Fay AxialCoordinate
randomAxialCoordinate = do
  orientation <- randomOrientation
  q <- randomI 100
  r <- randomI 100
  return
    AxialCoordinate { axial_q = q
                    , axial_r = r
                    , axial_orientation = orientation
                    }

randomOffsetCoordinate :: Fay OffsetCoordinate
randomOffsetCoordinate = do
  orientation <- randomOrientation
  offset <- randomOffset
  q <- randomI 100
  r <- randomI 100
  return $
    OffsetCoordinate { offset_q = q
                     , offset_r = r
                     , offset_orientation = orientation
                     , offset_offset = offset
                     }
