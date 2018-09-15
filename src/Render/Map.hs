{-# language RecordWildCards #-}
module Render.Map where

import Reflex
import Control.Lens.Getter ((^.))
import Graphics.Gloss (Picture, translate)
import Linear.V2 (R1(..), R2(..))

import Map (Map(..))
import Viewport (Viewport(..))

renderedMap :: Reflex t => Viewport t -> Map -> Behavior t Picture
renderedMap Viewport{..} Map{..} =
  (\vPos ->
     translate
       ((_mapWidth - _vpWidth) / 2 - vPos ^. _x)
       (-(_mapHeight - _vpHeight) / 2 + vPos ^. _y)
       _mapPicture) <$>
  current _vpPosition
