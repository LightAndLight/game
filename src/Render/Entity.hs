{-# language RecordWildCards #-}
module Render.Entity where

import Reflex

import Control.Lens.Getter ((^.))
import Graphics.Gloss (Picture, translate, blank)
import Linear.V2 (_x, _y)

import Dimensions (Width(..), Height(..), HasWidth(..), HasHeight(..))
import Entity.Picture (HasPicture(..))
import Entity.Position (HasPosition(..))
import Viewport (Viewport(..))

renderedEntity
  :: ( Reflex t
     , HasPosition t a, HasPicture t a
     , HasWidth a, HasHeight a
     )
  => Viewport t
  -> a
  -> Dynamic t Picture
renderedEntity Viewport{..} a =
  (\vPos ePos ePic ->
      if
        vPos^._x <= (ePos^._x + unWidth (a^.width)) &&
        ePos^._x <= (vPos^._x + unWidth _vpWidth) &&
        vPos^._y <= (ePos^._y + unHeight (a^.height)) &&
        ePos^._y <= (vPos^._y + unHeight _vpHeight)
      then
        translate
          ((unWidth (a^.width) - unWidth _vpWidth) / 2 - vPos^._x + ePos^._x)
          (-(unHeight (a^.height) - unHeight _vpHeight) / 2 + vPos^._y - ePos^._y)
          ePic
      else
        blank) <$>
  _vpPosition <*>
  (a^.position) <*>
  (a^.picture)
