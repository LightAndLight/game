{-# language RecordWildCards #-}
module Render.Entity where

import Reflex

import Control.Lens.Getter ((^.))
import Graphics.Gloss (Picture, translate, blank)
import Linear.V2 (R1(..), R2(..))

import Entity
  (HasEntity(..), entityWidth, entityHeight, entityPosition, entityPicture)
import Viewport (Viewport(..))

renderedEntity :: (Reflex t, HasEntity e) => Viewport t -> e t -> Behavior t Picture
renderedEntity Viewport{..} e =
  let
    ent = e ^. entity
  in
    (\vPos ePos ePic ->
       if
         vPos^._x < (ePos^._x + ent^.entityWidth) &&
         ePos^._x < (vPos^._x + _vpWidth) &&
         vPos^._y < (ePos^._y + ent^.entityHeight) &&
         ePos^._y < (vPos^._y + _vpHeight)
       then
         translate
           ((ent^.entityWidth - _vpWidth) / 2 - vPos^._x + ePos^._x)
           (-(ent^.entityHeight - _vpHeight) / 2 + vPos^._y - ePos^._y)
           ePic
       else
         blank) <$>
    current _vpPosition <*>
    current (ent^.entityPosition) <*>
    (ent^.entityPicture)
