{-# language FlexibleContexts #-}
{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language RecursiveDo #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
{-# language TupleSections #-}
module Player where

import Reflex

import Control.Lens.Getter ((^.))
import Control.Lens.TH (makeLenses)
import Control.Monad.Fix (MonadFix)
import Graphics.Gloss (Picture)
import Linear.V2 (V2, _x, _y)

import Controls (Controls(..))
import Dimensions (Width, Height, HasWidth(..), HasHeight(..))
import Entity.Picture (HasPicture(..))
import Entity.Position (HasPosition(..), mkEntityPosition)
import Entity.Quadrants (HasQuadrants(..))
import Grid.Quadrant (Quadrant)
import Map (Map)
import UniqueSupply.Class (UniqueSupply)

data Player t
  = Player
  { _playerQuadrants :: Dynamic t [Quadrant]
  , _playerPicture :: Dynamic t Picture
  , _playerPosition :: Dynamic t (V2 Float)
  , _playerWidth :: Width Float
  , _playerHeight :: Height Float
  , _playerInteract :: Event t (V2 Float)
  }
makeLenses ''Player
instance HasQuadrants t (Player t) where; quadrants = playerQuadrants
instance HasPosition t (Player t) where; position = playerPosition
instance HasPicture t (Player t) where; picture = playerPicture
instance HasWidth (Player t) where; width = playerWidth
instance HasHeight (Player t) where; height = playerHeight

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

  dPlayerPos <- mkEntityPosition mp w h pos eX eY

  pure dPlayerPos

mkPlayer
  :: ( MonadHold t m, MonadFix m
     , UniqueSupply t m
     , Adjustable t m
     )
  => Map
  -> Controls t
  -> Dynamic t [Quadrant]
  -> Picture
  -> Width Float
  -> Height Float
  -> V2 Float
  -> m (Player t)
mkPlayer mp controls _playerQuadrants pic _playerWidth _playerHeight pPos = do
  _playerPosition <- mkPlayerPos mp controls _playerWidth _playerHeight pPos

  let
    _playerPicture = pure pic
    _playerInteract = current _playerPosition <@ _eSpacePressed controls

  let player = Player{..}

  pure player
