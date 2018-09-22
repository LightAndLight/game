{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}
{-# language RecursiveDo #-}
module Entity.Player where

import Reflex

import Control.Lens.Getter ((^.))
import Control.Lens.Lens (lens)
import Control.Lens.Operators ((<&>))
import Control.Monad.Fix (MonadFix)
import Data.Foldable (fold)
import Data.Monoid (Endo(..))
import Graphics.Gloss (Picture)
import Linear.V2 (V2(..), R1(..), R2(..))

import Controls (Controls(..))
import Dimensions (Width, Height)
import Entity
  (HasEntity(..), Entity, entityPosition, mkEntityPos, mkMovingEntity)
import GridManager.Class (GridManager)
import Map (Map)

data Player t
  = Player
  { _playerEntity :: Entity t
  , _playerInteract :: Event t (V2 Float)
  }

instance HasEntity Player where
  entity = lens _playerEntity (\p e -> p { _playerEntity = e })

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
