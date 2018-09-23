{-# language DeriveFunctor #-}
{-# language LambdaCase #-}
{-# language RecordWildCards #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables #-}
module Viewport where

import Reflex
import Control.Lens.Getter ((^.))
import Control.Monad.Fix (MonadFix)
import Data.Monoid (Endo(..))
import Linear.V2 (V2(..), R1(..), R2(..))

import Dimensions (Width(..), Height(..))
import Entity (Entity(..))
import Map (Map(..))

data ScreenSize a = ScreenSize a a
  deriving Functor

data Viewport t
  = Viewport
  { _vpWidth :: Width Float
  , _vpHeight :: Height Float
  , _vpPosition :: Dynamic t (V2 Float)
  }

data ViewportConfig t
  = EdgePan
  { edgePanThreshold :: Float
  , edgePanTarget :: Entity t
  }
  | Controlled
  { controlledX :: Event t (Float -> Float)
  , controlledY :: Event t (Float -> Float)
  }

edgePanEvents
  :: Reflex t
  => ScreenSize Float
  -> Float -- ^ Edge-scrolling threshold
  -> Entity t
  -> (Event t (Endo Float), Event t (Endo Float))
edgePanEvents (ScreenSize vpW vpH) dist Entity{..} = (eX, eY)
  where
    eX =
      (\epos ->
         Endo $ \vx ->
         let
           toLeftEdge = (vx + dist) - epos^._x
           toRightEdge =
             (epos^._x + unWidth _entityWidth) -
             (vx + vpW - dist)
         in
           if toLeftEdge > 0
           then vx - toLeftEdge
           else
             if toRightEdge > 0
             then vx + toRightEdge
             else vx) <$>
      updated _entityPosition

    eY =
      (\epos ->
         Endo $ \vy ->
         let
           toTopEdge = (vy + dist) - epos^._y
           toBottomEdge =
             (epos^._y + unHeight _entityHeight) -
             (vy + vpH - dist)
         in
           if toTopEdge > 0
           then vy - toTopEdge
           else
             if toBottomEdge > 0
             then vy + toBottomEdge
             else vy) <$>
      updated _entityPosition

-- | Later configs in the list have higher precedence
mkViewport
  :: forall t m
   . (MonadHold t m, Reflex t, MonadFix m)
  => ScreenSize Float
  -> Map -- ^ The map it is viewing
  -> [ViewportConfig t]
  -> m (Viewport t)
mkViewport ss@(ScreenSize vpW vpH) Map{..} cfgs = do
  let
    _vpWidth = Width vpW
    _vpHeight = Height vpH

  rec
    vpX <-
      holdUniqDyn =<<
      holdDyn 0
        ((\vx fx -> max 0 . min (unWidth _mapWidth - vpW) $ fx vx) <$>
        current vpX <@> coerceEvent eX)
  rec
    vpY <-
      holdUniqDyn =<<
      holdDyn 0
      ((\vy fy -> max 0 . min (unHeight _mapHeight - vpH) $ fy vy) <$>
      current vpY <@> coerceEvent eY)

  let _vpPosition = V2 <$> vpX <*> vpY

  pure Viewport{..}

  where
    eX, eY :: Event t (Endo Float)
    (eX, eY) =
      foldMap
        (\case
            EdgePan dist e -> edgePanEvents ss dist e
            Controlled x y -> (coerceEvent x, coerceEvent y))
        cfgs
