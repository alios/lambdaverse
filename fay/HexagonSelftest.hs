module HexagonSelftest where

import           FFI
import           Hexagon
import           Hexagon.Test
import           Language.Fay.Yesod
import           Prelude
import           SharedTypes

main :: Fay ()
main = do
  putStrLn "Starting lamadevs hexagon tests"
  addWindowEvent "load" onLoad
  return ()

data Event

addWindowEvent :: String -> (Event -> Fay ()) -> Fay ()
addWindowEvent = ffi "window.addEventListener(%1, %2)"

onLoad :: Event -> Fay ()
onLoad _ = do
  putStrLn "Initializing ..."
  hexagonSpec

hexagonSpec :: Fay ()
hexagonSpec = do
  conversionSpec
  neighborsSpec
  distanceSpec
  diagonalsSpec
  rangeSpec



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
  qunit_test "testing neighbors by" $ \a -> do
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

distanceSpec :: Fay ()
distanceSpec = do
  cs1 <- sequence $ replicate 100 $ randomCubeCoordinate
  os1 <- sequence $ replicate 100 $ randomOffsetCoordinate
  as1 <- sequence $ replicate 100 $ randomAxialCoordinate
  cs2 <- sequence $ replicate 100 $ randomCubeCoordinate
  os2 <- sequence $ replicate 100 $ randomOffsetCoordinate
  as2 <- sequence $ replicate 100 $ randomAxialCoordinate
  qunit_test "testing distance by" $ \a -> do
    test2Prop a "comapring cubeDistance to axialDistance from cube"
        prop_distance_cube_axial cs1 cs2
    test2Prop a "comapring cubeDistance to offsetDistance (odd) from cube"
        (prop_distance_cube_offset Odd) cs1 cs2
    test2Prop a "comapring cubeDistance to offsetDistance (even) from cube"
        (prop_distance_cube_offset Even) cs1 cs2
    test2Prop a "comapring axialDistance to cubeDistance from axial"
        prop_distance_axial_cube as1 as2
    test2Prop a "comapring axialDistance to offsetDistance (odd) from axial"
        (prop_distance_axial_offset Odd) as1 as2
    test2Prop a "comapring axialDistance to offsetDistance (even) from axial"
        (prop_distance_axial_offset Even) as1 as2
    test2Prop a "comapring offsetDistance to axialDistance from offset"
        prop_distance_offset_axial os1 os2
    test2Prop a "comapring offsetDistance to cubeDistance from offset"
        prop_distance_offset_cube os1 os2

rangeSpec :: Fay ()
rangeSpec = do
  cs <- sequence $ replicate 100 $ randomCubeCoordinate
  os <- sequence $ replicate 100 $ randomOffsetCoordinate
  as <- sequence $ replicate 100 $ randomAxialCoordinate
  rs' <- sequence $ replicate 10 $ randomPos 10
  let rs = map abs rs'
  qunit_test "range calc produces cube coordinates" $ \a -> do
    test2Prop a "which are valid" prop_range_valid rs cs
  qunit_test "range returns (3n^2 - 3n + 1) | n' = n-1 coordinates" $ \a -> do
    test2Prop a "with CubeCoordinate" prop_range_lengthC' rs cs
    test2Prop a "with AxialCoordinate" prop_range_lengthA' rs as
    test2Prop a "with OffsetCoordinate" prop_range_lengthO' rs os
  qunit_test "range 1 equals neighbors c + c" $ \a -> do
    testProp a "with CubeCoordinate" prop_range_neighborsC cs
    testProp a "with AxialCoordinate" prop_range_neighborsA as
    testProp a "with OffsetCoordinate" prop_range_neighborsO os

diagonalsSpec :: Fay ()
diagonalsSpec = do
  cs <- sequence $ replicate 100 $ randomCubeCoordinate
  os <- sequence $ replicate 100 $ randomOffsetCoordinate
  as <- sequence $ replicate 100 $ randomAxialCoordinate
  qunit_test "testing diagonals by" $ \a -> do
    testProp a "comapring cubeDiagonals to axialDiagonals from cube" prop_diagonals_cube_axial cs
    testProp a "comapring cubeDiagonals to offsetDiagonals (odd) from cube"
      (prop_diagonals_cube_offset Odd) cs
    testProp a "comapring cubeDiagonals to offsetDiagonals (even) from cube"
      (prop_diagonals_cube_offset Even) cs
    testProp a "comapring axialDiagonals to cubeDiagonals from axial" prop_diagonals_axial_cube as
    testProp a "comapring axialDiagonals to offsetDiagonals (odd) from axial"
      (prop_diagonals_axial_offset Odd) as
    testProp a "comapring axialDiagonals to offsetDiagonals (even) from axial"
      (prop_diagonals_axial_offset Even) as
    testProp a "comapring offsetDiagonals to axialDiagonals from offset" prop_diagonals_offset_axial os
    testProp a "comapring offsetDiagonals to cubeDiagonals from offset" prop_diagonals_offset_cube os


data Assert
qunit_test :: String -> (Assert -> Fay ()) -> Fay ()
qunit_test = ffi "QUnit.test( %1, %2 )"

assert_ok :: Assert -> Bool -> String -> Fay ()
assert_ok = ffi "%1.ok( %2, %3 )"

testProp a desc p cs  =
  let ps = and $ map p cs
  in assert_ok a ps desc

test2Prop :: Assert -> String -> (a -> b -> Bool) -> [a] -> [b] -> Fay ()
test2Prop a desc p cs ds =
  let _test2Prop [] = []
      _test2Prop ((c,d):xs) = (p c d) : (_test2Prop xs)
  in assert_ok a (and $ _test2Prop $ zip cs ds) desc


randomD' :: Fay Double
randomD' = ffi "window.Math.random()"


randomB :: Fay Bool
randomB = do
  d <- randomD'
  return $ d > 0.5

randomD :: Double -> Fay Double
randomD n = do
  d <- randomD'
  s <- randomB
  return $ if s then d * n else d * n * (-1.0)

randomPos n = do
  i <- randomI n
  return $ abs i

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
