{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}
{-# language RecursiveDo #-}
module Entity.Box where

import Reflex
import Control.Lens.Lens (lens)
import Control.Monad.Fix (MonadFix)
import Graphics.Gloss (Picture)

import Entity (HasEntity(..), Entity, MkEntity, mkStaticEntity, intersects)
import Entity.Player (Player(..))
import GridManager.Class (GridManager)
import UniqueSupply.Class (UniqueSupply)

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
     , GridManager t (Entity t) m, UniqueSupply t m
     )
  => Event t ()
  -> MkEntity
  -> Player t
  -> (Picture, Picture) -- ^ (closed picture, open picture)
  -> m (Box t)
mkBox eAdd mkE player (closedPic, openPic) = mdo
  let ePic = (\b -> if b then openPic else closedPic) <$> updated _boxOpen

  _boxEntity <- mkStaticEntity eAdd mkE ePic

  _boxOpen <-
    holdDyn False $
    fforMaybe
      ((,) <$>
       current _boxOpen <*>
       current (intersects _boxEntity player) <@ _playerInteract player)
      (\(open, touching) ->
         if touching
         then Just $ not open
         else Nothing)

  pure Box{..}
