module Map where

import Graphics.Gloss (Picture)

data Map
  = Map
  { _mapPicture :: Picture
  , _mapWidth :: Float
  , _mapHeight :: Float
  }
