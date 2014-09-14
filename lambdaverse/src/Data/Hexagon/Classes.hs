
module Data.Hexagon.Classes where

class Coordinate c where
  neighbors :: c -> [c]
  diagonals :: c -> [c]
  distance :: c -> c -> Double
  line :: c -> c -> [c]
  range :: Int -> c -> [c]
