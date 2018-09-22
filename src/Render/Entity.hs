{-# language RecordWildCards #-}
module Render.Entity where

import Reflex

import Control.Lens.Getter ((^.))
import Graphics.Gloss (Picture, translate, blank)
import Linear.V2 (_x, _y)

import Dimensions (Width(..), Height(..))
import Entity (Entity(..))
import Viewport (Viewport(..))

renderedEntity
  :: Reflex t
  => Viewport t
  -> Entity t
  -> Dynamic t Picture
renderedEntity Viewport{..} Entity{..} =
  (\vPos ePos ePic ->
      if
        vPos^._x <= ePos^._x &&
        ePos^._x <= (vPos^._x + unWidth _vpWidth) &&
        vPos^._y <= ePos^._y &&
        ePos^._y <= (vPos^._y + unHeight _vpHeight)
      then
        translate
          ((unWidth _entityWidth - unWidth _vpWidth) / 2 - vPos^._x + ePos^._x)
          (-(unHeight _entityHeight - unHeight _vpHeight) / 2 + vPos^._y - ePos^._y)
          ePic
      else
        blank) <$>
  _vpPosition <*>
  _entityPosition <*>
  _entityPicture
