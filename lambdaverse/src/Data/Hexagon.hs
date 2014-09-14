{-# LANGUAGE DeriveDataTypeable #-}

module Data.Hexagon where

import           Data.Data
import           Data.Hexagon.AxialCoordinate
import           Data.Hexagon.CubeCoordinate
import           Data.Hexagon.Hexgrid
import           Data.Hexagon.OffsetCoordinate
import           Data.Hexagon.ScreenCoordinate
import           Data.Hexagon.Types
import           Prelude





--
-- The Hexgrid
--


hexDim :: Hexgrid -> ScreenCoordinate
hexDim g = ScreenCoordinate (round $ hexWidth g) (round $ hexHeight g)

hexDist :: Hexgrid -> ScreenCoordinate
hexDist g = ScreenCoordinate (round $ hexHorizontalDist g) (round $ hexVerticalDist g)




