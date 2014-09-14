{-# LANGUAGE DeriveDataTypeable #-}

module Data.Hexagon.ScreenCoordinate where

import           Data.Data
import           Data.Hexagon.AxialCoordinate
import           Data.Hexagon.CubeCoordinate
import           Data.Hexagon.OffsetCoordinate
import           Data.Hexagon.Types
import           Prelude

data ScreenCoordinate =
  ScreenCoordinate
    { _pixel_x :: Int
    , _pixel_y :: Int
    } deriving (Show, Read, Eq, Typeable, Data)

data ScreenBox =
  ScreenBox
    { viewbox_pos  :: ScreenCoordinate
    , viewbox_size :: ScreenCoordinate
    } deriving (Show, Read, Eq, Typeable, Data)

axial2offset :: Offset -> AxialCoordinate -> OffsetCoordinate
axial2offset o = cube2offset o .  axial2cube

offset2axial :: OffsetCoordinate -> AxialCoordinate
offset2axial = cube2axial . offset2cube



mkScreenBox :: Int -> Int -> Int -> Int -> ScreenBox
mkScreenBox x y w h =
  ScreenBox { viewbox_pos  = ScreenCoordinate { _pixel_x = x, _pixel_y = y }
            , viewbox_size = ScreenCoordinate { _pixel_x = w, _pixel_y = h }
            }


multScreen :: Double -> ScreenCoordinate -> ScreenCoordinate
multScreen d s =
  let vmul a = round $ d * (fromIntegral a)
  in ScreenCoordinate { _pixel_x = vmul $ _pixel_x s
                      , _pixel_y = vmul $ _pixel_y s
                      }

plusScreen :: ScreenCoordinate -> ScreenCoordinate -> ScreenCoordinate
plusScreen a b
  = ScreenCoordinate { _pixel_x = _pixel_x a + _pixel_x b
                     , _pixel_y = _pixel_y a + _pixel_y b
                     }
