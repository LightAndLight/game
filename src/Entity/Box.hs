{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}
{-# language RecursiveDo #-}
module Entity.Box where

import Reflex
import Control.Lens.Lens (lens)
import Control.Monad.Fix (MonadFix)
import Control.Monad.State.Strict (MonadState)
import Graphics.Gloss (Picture)

import Entity (HasEntity(..), Entity, mkStaticEntity, intersects)
import Entity.Player (Player(..))
import Grid (HasGrid(..))
import Map (Map(..))
import Unique (HasSupply(..))

data Box t
  = Box
  { _boxEntity :: Entity t
  , _boxOpen :: Dynamic t Bool
  }

instance HasEntity Box where
  entity = lens _boxEntity (\b e -> b { _boxEntity = e })

{-
a box can only be opened when the thing that tried to open it is close enough

if a the thing is close to the box
then some pre-determined event will be considered a command to open or close the box
-}
mkBox
  :: ( Reflex t, MonadHold t m, MonadFix m
     , HasGrid s t (Entity t), HasSupply s, MonadState s m
     )
  => Map -- ^ map on which the entity resides
  -> Player t -- ^ the player
  -> (Picture, Picture) -- ^ (closed picture, open picture)
  -> Float -- ^ x coordinate
  -> Float -- ^ y coordinate
  -> Float -- ^ width
  -> Float -- ^ height
  -> m (Box t)
mkBox mp Player{..} (closedPic, openPic) x y w h = mdo
  let bPic = (\b -> if b then openPic else closedPic) <$> current _boxOpen

  _boxEntity <- mkStaticEntity mp bPic x y w h

  _boxOpen <-
    holdDyn False $
    fforMaybe
      ((,) <$>
       current _boxOpen <*>
       current (_playerEntity `intersects` box) <@ _playerInteract)
      (\(open, touching)->
         if touching
         then Just $ not open
         else Nothing)

  let box = Box{..}

  pure box
