module Map where

import Graphics.Gloss (Picture)

import Dimensions (Width(..), Height(..))

data Map
  = Map
  { _mapPicture :: Picture
  , _mapWidth :: Width Float
  , _mapHeight :: Height Float
  }
