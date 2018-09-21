{-# language RecordWildCards #-}
module Render.Entity where

import Reflex

import Control.Lens.Getter ((^.))
import Graphics.Gloss (Picture, translate, blank)
import Linear.V2 (V2, R1(..), R2(..))

import Dimensions (Width(..), Height(..))
import Entity
  (HasEntity(..), entityWidth, entityHeight, entityPosition, entityPicture)
import Viewport (Viewport(..))

renderedEntity
  :: Reflex t
  => Viewport t
  -> (Width Float, Height Float, Dynamic t (V2 Float))
  -> Dynamic t Picture
  -> Dynamic t Picture
renderedEntity Viewport{..} (w, h, dPosition) dPicture =
  (\vPos ePos ePic ->
      if
        vPos^._x <= ePos^._x &&
        ePos^._x <= (vPos^._x + unWidth _vpWidth) &&
        vPos^._y <= ePos^._y &&
        ePos^._y <= (vPos^._y + unHeight _vpHeight)
      then
        translate
          ((unWidth w - unWidth _vpWidth) / 2 - vPos^._x + ePos^._x)
          (-(unHeight h - unHeight _vpHeight) / 2 + vPos^._y - ePos^._y)
          ePic
      else
        blank) <$>
  _vpPosition <*>
  dPosition <*>
  dPicture
