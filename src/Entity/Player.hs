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
  (HasEntity(..), Entity, MkEntity, entityPosition, getMkEntity, mkMovingEntity)
import GridManager.Class (GridManager)
import Map (Map)
import UniqueSupply.Class (UniqueSupply)

data Player t
  = Player
  { _playerEntity :: Entity t
  , _playerInteract :: Event t (V2 Float)
  }

instance HasEntity Player where
  entity = lens _playerEntity (\p e -> p { _playerEntity = e })

mkPlayer
  :: ( MonadHold t m, Reflex t, MonadFix m
     , GridManager t (Entity t) m, UniqueSupply t m
     )
  => Event t ()
  -> MkEntity
  -> Controls t
  -> m (Player t)
mkPlayer eAdd mkE Controls{..} = mdo
  let
    eX =
      (.) <$>
      ((\b n -> if b then n+5 else n) <$> current _dDHeld) <*>
      ((\b n -> if b then n-5 else n) <$> current _dAHeld) <@>
      ((^. _x) <$> bPlayerPos <@ _eRefresh)

    eY =
      (.) <$>
      ((\b n -> if b then n+5 else n) <$> current _dSHeld) <*>
      ((\b n -> if b then n-5 else n) <$> current _dWHeld) <@>
      ((^. _y) <$> bPlayerPos <@ _eRefresh)

  _playerEntity <- mkMovingEntity eAdd mkE never eX eY

  let
    bPlayerPos = current $ _playerEntity^.entityPosition
    _playerInteract = bPlayerPos <@ _eSpacePressed
    player = Player{..}

  pure player
