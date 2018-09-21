{-# language RecordWildCards #-}
module Render.Map where

import Reflex
import Control.Lens.Getter ((^.))
import Graphics.Gloss (Picture, translate)
import Linear.V2 (R1(..), R2(..))

import Dimensions (Width(..), Height(..))
import Map (Map(..))
import Viewport (Viewport(..))

renderedMap :: Reflex t => Viewport t -> Map -> Dynamic t Picture
renderedMap Viewport{..} Map{..} =
  (\vPos ->
     translate
       ((unWidth _mapWidth - unWidth _vpWidth) / 2 - vPos ^. _x)
       (-(unHeight _mapHeight - unHeight _vpHeight) / 2 + vPos ^. _y)
       _mapPicture) <$>
  _vpPosition
