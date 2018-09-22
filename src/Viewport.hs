{-# language RecordWildCards #-}
{-# language RecursiveDo #-}
module Viewport where

import Reflex

import Control.Lens.Getter ((^.))
import Control.Monad.Fix (MonadFix)
import Linear.V2 (V2(..), R1(..), R2(..))

import Controls (Controls(..))
import Dimensions (Width(..), Height(..))
import Map (Map(..))
import Player (Player(..))

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
  -> Map -- ^ The map it is viewing
  -> Controls t -- ^ The controls
  -> Player t
  -> m (Viewport t)
mkViewport dist (ScreenSize (vpW, vpH)) Map{..} Controls{..} Player{..} = mdo

  let
    _vpWidth = Width vpW
    _vpHeight = Height vpH

  vpX <-
    holdUniqDyn =<<
    holdDyn 0
    ((\epos vpos ->
        let
          toLeftEdge = (vpos + dist) - epos^._x

          toRightEdge =
            (epos^._x + unWidth _playerWidth) -
            (vpos + unWidth _vpWidth - dist)
        in
          if toLeftEdge > 0
          then max 0 $ vpos - toLeftEdge
          else
            if toRightEdge > 0
            then min (unWidth _mapWidth - unWidth _vpWidth) $ vpos + toRightEdge
            else vpos) <$>
     current _playerPosition <*>
     current vpX <@ _eRefresh)

  vpY <-
    holdUniqDyn =<<
    holdDyn 0
    ((\epos vpos ->
        let
          toTopEdge = (vpos + dist) - epos^._y

          toBottomEdge =
            (epos^._y + unHeight _playerHeight) -
            (vpos + unHeight _vpHeight - dist)
        in
          if toTopEdge > 0
          then max 0 $ vpos - toTopEdge
          else
            if toBottomEdge > 0
            then min (unHeight _mapHeight - unHeight _vpHeight) $ vpos + toBottomEdge
            else vpos) <$>
     current _playerPosition <*>
     current vpY <@ _eRefresh)

  let _vpPosition = V2 <$> vpX <*> vpY

  pure Viewport{..}
