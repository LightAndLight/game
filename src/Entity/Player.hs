{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}
{-# language RecursiveDo #-}
module Entity.Player where

import Reflex

import Control.Lens.Getter ((^.))
import Control.Lens.Lens (lens)
import Control.Monad.Fix (MonadFix)
import Control.Monad.State.Strict (MonadState)
import Data.Foldable (fold)
import Data.Monoid (Endo(..))
import Graphics.Gloss (Picture)
import Linear.V2 (V2, R1(..), R2(..))

import Controls (Controls(..))
import Entity (HasEntity(..), Entity, entityPosition, mkMovingEntity)
import Grid (HasGrid(..))
import Map (Map)
import Unique (HasSupply)

data Player t
  = Player
  { _playerEntity :: Entity t
  , _playerInteract :: Event t (V2 Float)
  }

instance HasEntity Player where
  entity = lens _playerEntity (\p e -> p { _playerEntity = e })

mkPlayer
  :: ( MonadHold t m, Reflex t, MonadFix m
     , HasGrid s t (Entity t), HasSupply s, MonadState s m
     )
  => Controls t
  -> Map
  -> Picture
  -> m (Player t)
mkPlayer Controls{..} mp pic = mdo
  dXPos <-
    holdUniqDyn =<<
    holdDyn 0
      (appEndo
       (fold
          [ Endo $ \bp -> (\n b -> if b then n+5 else n) <$> bp <*> current _dDHeld
          , Endo $ \bp -> (\n b -> if b then n-5 else n) <$> bp <*> current _dAHeld
          ])
       ((^. _x) <$> current (player^.entity.entityPosition))
       <@ _eRefresh)

  dYPos <-
    holdUniqDyn =<<
    holdDyn 0
      (appEndo
       (fold
          [ Endo $ \bp -> (\n b -> if b then n+5 else n) <$> bp <*> current _dSHeld
          , Endo $ \bp -> (\n b -> if b then n-5 else n) <$> bp <*> current _dWHeld
          ])
       ((^. _y) <$> current (player^.entity.entityPosition))
       <@ _eRefresh)

  _playerEntity <- mkMovingEntity mp (pure pic) dXPos dYPos 20 20
  let _playerInteract = current (_playerEntity^.entityPosition) <@ _eSpacePressed

  let player = Player{..}

  pure player
