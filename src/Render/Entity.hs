{-# language RecordWildCards #-}
module Render.Entity where

import Reflex

import Control.Lens.Getter ((^.), to)
import Graphics.Gloss (Picture, translate, blank)
import Linear.V2 (R1(..), R2(..))

import Dimensions (Width(..), Height(..))
import Entity
  (HasEntity(..), entityWidth, entityHeight, entityPosition, entityPicture)
import Viewport (Viewport(..))

renderedEntity
  :: (Reflex t, HasEntity e)
  => Viewport t
  -> e t
  -> Dynamic t Picture
renderedEntity Viewport{..} e =
  let
    ent = e ^. entity
  in
    (\vPos ePos ePic ->
       if
         vPos^._x <= ePos^._x &&
         ePos^._x <= (vPos^._x + unWidth _vpWidth) &&
         vPos^._y <= ePos^._y &&
         ePos^._y <= (vPos^._y + unHeight _vpHeight)
       then
         translate
           ((ent^.entityWidth.to unWidth - unWidth _vpWidth) / 2 - vPos^._x + ePos^._x)
           (-(ent^.entityHeight.to unHeight - unHeight _vpHeight) / 2 + vPos^._y - ePos^._y)
           ePic
       else
         blank) <$>
    _vpPosition <*>
    (ent^.entityPosition) <*>
    (ent^.entityPicture)

renderedEntityE
  :: (Reflex t, MonadHold t m, HasEntity e)
  => Event t (Viewport t)
  -> Event t (e t)
  -> m (Dynamic t Picture)
renderedEntityE eVp eE = do
  dMaybeVp <- holdDyn Nothing $ Just <$> eVp
  dMaybeE <- holdDyn Nothing $ Just <$> eE
  pure $ do
    v <- dMaybeVp
    e <- dMaybeE
    flip (maybe $ pure blank) v $ \v' ->
      flip (maybe $ pure blank) e $ \e' ->
      renderedEntity v' e'
