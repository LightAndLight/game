{-# language FlexibleContexts #-}
{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language RecursiveDo #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language TupleSections #-}
module Player where

import Reflex

import Control.Lens.Getter (view)
import Control.Lens.TH (makeLenses)
import Control.Monad.Fix (MonadFix)
import Graphics.Gloss (Picture)
import Linear.Metric (normalize)
import Linear.V2 (V2(..), _x, _y)

import Controls (Controls(..))
import Dimensions (Width, Height, HasWidth(..), HasHeight(..))
import Entity.Picture (HasPicture(..))
import Entity.Position (HasPosition(..), mkEntityPosition)
import Entity.Quadrants (HasQuadrants(..))
import Grid (GridConfig, getQuadrants'')
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
  :: forall t m
   . (Reflex t, MonadHold t m, MonadFix m)
  => Map
  -> Controls t
  -> Width Float
  -> Height Float
  -> V2 Float
  -> m (Dynamic t (V2 Float))
mkPlayerPos mp Controls{..} w h pos = mdo
  let
    dNewPos :: Dynamic t (V2 Float)
    dNewPos =
      (+) <$>
      dPlayerPos <*>
      (fmap ((*5) . normalize . sum) $
       sequence
       [ (\b -> if b then V2 1 0 else 0) <$> _dDHeld
       , (\b -> if b then V2 (-1) 0 else 0) <$> _dAHeld
       , (\b -> if b then V2 0 (-1) else 0) <$> _dWHeld
       , (\b -> if b then V2 0 1 else 0) <$> _dSHeld
       ])

    eX = view _x <$> current dNewPos <@ _eRefresh
    eY = view _y <$> current dNewPos <@ _eRefresh

  dPlayerPos <- mkEntityPosition mp w h pos eX eY

  pure dPlayerPos

mkPlayer
  :: ( MonadHold t m, MonadFix m
     , UniqueSupply t m
     , Adjustable t m
     )
  => Map
  -> GridConfig
  -> Controls t
  -> Picture
  -> Width Float
  -> Height Float
  -> V2 Float
  -> m (Player t)
mkPlayer mp gc controls pic _playerWidth _playerHeight pPos = do
  _playerPosition <- mkPlayerPos mp controls _playerWidth _playerHeight pPos

  let
    _playerPicture = pure pic
    _playerInteract = current _playerPosition <@ _eSpacePressed controls
    _playerQuadrants = getQuadrants'' gc player
    player = Player{..}

  pure player
