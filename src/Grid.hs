{-# language DeriveFunctor, StandaloneDeriving #-}
module Grid where

import Reflex

import Dimensions (Width(..), Height(..))
import UniqueMap (UniqueMap)

data Grid t a
  = Grid
  { _gridWidth :: Width Float -- ^ width
  , _gridHeight :: Height Float -- ^ height
  , _gridVerticalCenter :: Float -- ^ vertical center
  , _gridHorizontalCenter :: Float -- ^ horizontal center
  , _gridTopLeft :: Dynamic t (UniqueMap a) -- ^ top left
  , _gridTopRight :: Dynamic t (UniqueMap a) -- ^ top right
  , _gridBottomLeft :: Dynamic t (UniqueMap a) -- ^ bottom left
  , _gridBottomRight :: Dynamic t (UniqueMap a) -- ^ bottom right
  }
deriving instance Reflex t => Functor (Grid t)

data Quadrant = TL | TR | BL | BR deriving (Eq, Show)

