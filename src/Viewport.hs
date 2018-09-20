{-# language RecordWildCards #-}
{-# language RecursiveDo #-}
module Viewport where

import Reflex

import Control.Lens.Getter ((^.), to)
import Control.Monad.Fix (MonadFix)
import Linear.V2 (V2(..), R1(..), R2(..))

import Controls (Controls(..))
import Dimensions (Width(..), Height(..))
import Entity (entity, entityWidth, entityHeight, entityPosition)
import Entity.Player (Player(..))
import Map (Map(..))

newtype ScreenSize a = ScreenSize { unScreenSize :: (a, a) }

instance Functor ScreenSize where
  fmap f (ScreenSize (a, b)) = ScreenSize (f a, f b)

data Viewport t
  = Viewport
  { _vpWidth :: Width Float
  , _vpHeight :: Height Float
  , _vpPosition :: Dynamic t (V2 Float)
  }

mkViewport
  :: (MonadHold t m, Reflex t, MonadFix m)
  => Float -- ^ Edge-scrolling threshold
  -> ScreenSize Float
  -> Controls t -- ^ The controls
  -> Player t
  -> Map -- ^ The map it is viewing
  -> m (Viewport t)
mkViewport threshold (ScreenSize (vpW, vpH)) Controls{..} Player{..} Map{..} = mdo
  let
    _vpWidth = Width vpW
    _vpHeight = Height vpH

  vpX <-
    holdUniqDyn =<<
    holdDyn 0
    ((\epos vpos ->
        let
          toLeftEdge = (vpos + threshold) - epos^._x

          toRightEdge =
            (epos^._x + _playerEntity^.entityWidth.to unWidth) -
            (vpos + unWidth _vpWidth - threshold)
        in
          if toLeftEdge > 0
          then max 0 $ vpos - toLeftEdge
          else
            if toRightEdge > 0
            then min (unWidth _mapWidth - unWidth _vpWidth) $ vpos + toRightEdge
            else vpos) <$>
     current (_playerEntity^.entityPosition) <*>
     current vpX <@ _eRefresh)

  vpY <-
    holdUniqDyn =<<
    holdDyn 0
    ((\epos vpos ->
        let
          toTopEdge = (vpos + threshold) - epos^._y

          toBottomEdge =
            (epos^._y + _playerEntity^.entityHeight.to unHeight) -
            (vpos + unHeight _vpHeight - threshold)
        in
          if toTopEdge > 0
          then max 0 $ vpos - toTopEdge
          else
            if toBottomEdge > 0
            then min (unHeight _mapHeight - unHeight _vpHeight) $ vpos + toBottomEdge
            else vpos) <$>
     current (_playerEntity^.entityPosition) <*>
     current vpY <@ _eRefresh)

  let _vpPosition = V2 <$> vpX <*> vpY

  pure Viewport{..}
