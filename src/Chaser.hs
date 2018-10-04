{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language TemplateHaskell #-}
{-# language RecordWildCards #-}
{-# language RecursiveDo #-}
module Chaser where

import Reflex

import Control.Lens.Getter ((^.), view)
import Control.Lens.TH (makeLenses)
import Control.Monad.Fix (MonadFix)
import Entity.Intersects (intersects)
import Entity.Picture (HasPicture(..))
import Entity.Position (HasPosition(..))
import Entity.Quadrants (HasQuadrants(..))
import Graphics.Gloss (Picture, rotate)
import Linear.Affine (Point(..), qdA, origin)
import Linear.Metric (dot,normalize)
import Linear.V2 (V2(..), _x, _y)

import Animate (loopWithDelay)
import Controls (Controls(..))
import Dimensions (Width, Height, HasWidth(..), HasHeight(..))
import Entity.Position (mkEntityPosition)
import Grid (GridConfig, getQuadrants'')
import Grid.Quadrant (Quadrant)
import Map (Map)
import UniqueSupply.Class (UniqueSupply)
import Player (Player)

data Chaser t
  = Chaser
  { _chaserQuadrants :: Dynamic t [Quadrant]
  , _chaserPicture :: Dynamic t Picture
  , _chaserRotation :: Dynamic t Float
  , _chaserPosition :: Dynamic t (V2 Float)
  , _chaserWidth :: Width Float
  , _chaserHeight :: Height Float
  , _chaserCaughtPlayer :: Event t ()
  }
makeLenses ''Chaser
instance HasQuadrants t (Chaser t) where; quadrants = chaserQuadrants
instance HasPosition t (Chaser t) where; position = chaserPosition
instance HasPicture t (Chaser t) where; picture = chaserPicture
instance HasWidth (Chaser t) where; width = chaserWidth
instance HasHeight (Chaser t) where; height = chaserHeight

mkChaserPos
  :: (Reflex t, MonadHold t m, MonadFix m)
  => Map
  -> Controls t
  -> Width Float
  -> Height Float
  -> V2 Float
  -> Dynamic t (V2 Float)
  -> m (Dynamic t (V2 Float))
mkChaserPos mp Controls{..} w h initial dTargetPos = mdo
  let
    dNewPos =
      (\ppos cpos -> cpos + normalize (ppos - cpos) * 3.5) <$>
      dTargetPos <*>
      dPos

    eX = view _x <$> current dNewPos <@ _eRefresh
    eY = view _y <$> current dNewPos <@ _eRefresh

  dPos <- mkEntityPosition mp w h initial eX eY

  pure dPos

mkChaserRotation
  :: Reflex t
  => Dynamic t (V2 Float)
  -> Dynamic t (V2 Float)
  -> Dynamic t Float
mkChaserRotation dChaserPos dTargetPos =
  (\cpos dpos ->
      let
        diff = dpos - cpos
      in
        -- angleBetween computes positive radians, so multiply by the sign
        -- of the diff's y component
        signum (diff^._y) *
        angleBetween (V2 1 0) diff) <$>
  dChaserPos <*>
  dTargetPos
  where
    mag :: V2 Float -> Float
    mag = sqrt . qdA origin . P

    angleBetween :: V2 Float -> V2 Float -> Float
    angleBetween a b = acos ((a `dot` b) / (mag a * mag b))

mkChaserPicture
  :: (Reflex t, MonadHold t m, MonadFix m)
  => (Picture, Picture)
  -> Dynamic t Float
  -> Event t ()
  -> m (Dynamic t Picture)
mkChaserPicture (openPic, closedPic) dAngle eRefresh = do
  dPic <- loopWithDelay 10 [openPic, closedPic] eRefresh
  pure $ rotate . fromRadians <$> dAngle <*> dPic
  where
    fromRadians x = 180 * x / pi

mkChaser
  :: ( MonadHold t m, MonadFix m
     , UniqueSupply t m
     , Adjustable t m
     )
  => Map
  -> GridConfig
  -> Controls t
  -> (Picture, Picture)
  -> Width Float
  -> Height Float
  -> V2 Float
  -> Player t
  -> m (Chaser t)
mkChaser mp gc controls pic _chaserWidth _chaserHeight pPos player = do
  _chaserPosition <-
    mkChaserPos mp controls _chaserWidth _chaserHeight pPos (player^.position)

  let
    _chaserRotation = mkChaserRotation _chaserPosition (player^.position)
  _chaserPicture <-
    mkChaserPicture pic _chaserRotation (() <$ _eRefresh controls)
  rec
    _chaserCaughtPlayer <-
      fmap (() <$) . headE .
      ffilter id . updated $
      intersects chaser player
    let
      _chaserQuadrants = getQuadrants'' gc chaser
      chaser = Chaser{..}

  pure chaser
