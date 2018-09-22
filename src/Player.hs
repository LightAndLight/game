{-# language FlexibleContexts #-}
{-# language RecursiveDo #-}
{-# language RecordWildCards #-}
module Player where

import Reflex

import Control.Lens.Getter ((^.))
import Control.Monad (join)
import Control.Monad.Fix (MonadFix)
import Graphics.Gloss (Picture)
import Linear.V2 (V2, _x, _y)

import Controls (Controls(..))
import Dimensions (Width, Height)
import Entity (Entity, mkMovingEntity, mkEntityPos, entityQuadrants)
import Grid (Quadrant)
import GridManager.Class (GridManager)
import Map (Map)
import UniqueSupply.Class (UniqueSupply, requestUnique)

data Player t
  = Player
  { _playerQuadrants :: Dynamic t [Quadrant]
  , _playerPicture :: Dynamic t Picture
  , _playerPosition :: Dynamic t (V2 Float)
  , _playerWidth :: Width Float
  , _playerHeight :: Height Float
  , _playerInteract :: Event t (V2 Float)
  }

mkPlayerPos
  :: (Reflex t, MonadHold t m, MonadFix m)
  => Map
  -> Controls t
  -> Width Float
  -> Height Float
  -> V2 Float
  -> m (Dynamic t (V2 Float))
mkPlayerPos mp Controls{..} w h pos = mdo
  let
    eX =
      (.) <$>
      ((\b n -> if b then n+5 else n) <$> current _dDHeld) <*>
      ((\b n -> if b then n-5 else n) <$> current _dAHeld) <@>
      ((^. _x) <$> current dPlayerPos <@ _eRefresh)

    eY =
      (.) <$>
      ((\b n -> if b then n+5 else n) <$> current _dSHeld) <*>
      ((\b n -> if b then n-5 else n) <$> current _dWHeld) <@>
      ((^. _y) <$> current dPlayerPos <@ _eRefresh)

  dPlayerPos <- mkEntityPos mp w h pos eX eY

  pure dPlayerPos

mkPlayer
  :: ( MonadHold t m, MonadFix m
     , UniqueSupply t m, GridManager t (Entity t) m
     , Adjustable t m
     )
  => Map
  -> Controls t
  -> Event t a
  -> Picture
  -> Width Float
  -> Height Float
  -> V2 Float
  -> m (Player t)
mkPlayer mp controls eCreate pic _playerWidth _playerHeight pPos = do
  -- Make a player
  _playerPosition <- mkPlayerPos mp controls _playerWidth _playerHeight pPos
  let
    _playerPicture = pure pic
    _playerInteract = current _playerPosition <@ _eSpacePressed controls

  eUnique <- requestUnique eCreate

  (_, ePlayerEntity) <-
    runWithReplace
      (pure ())
      ((\u ->
          mkMovingEntity
            u
            _playerWidth
            _playerHeight
            _playerPicture
            _playerPosition) <$>
       eUnique)

  _playerQuadrants <-
    join <$> holdDyn (pure []) ((^.entityQuadrants) <$> ePlayerEntity)

  pure Player{..}
