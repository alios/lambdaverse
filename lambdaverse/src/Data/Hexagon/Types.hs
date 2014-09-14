{-# LANGUAGE DeriveDataTypeable #-}

module Data.Hexagon.Types where

import           Data.Data
import           Prelude


data Orientation = Horizontal | Vertical
    deriving (Show, Read, Eq, Typeable, Data)

data Offset = Odd | Even
    deriving (Show, Read, Eq, Typeable, Data)
