{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}
{-# language RecursiveDo #-}
module Entity.Box where

import Reflex
import Control.Lens.Lens (lens)
import Control.Monad.Fix (MonadFix)
import Graphics.Gloss (Picture)
import Linear.V2 (V2)

import Dimensions (Width, Height)
import Entity (HasEntity(..), Entity, intersects)
import Grid (Quadrant)
import GridManager.Class (GridManager)

data Box t
  = Box
  { _boxEntity :: Entity t
  , _boxOpen :: Dynamic t Bool
  }

instance HasEntity Box where
  entity = lens _boxEntity (\b e -> b { _boxEntity = e })


mkBoxOpen
  :: (Reflex t, MonadHold t m, MonadFix m)
  => (Dynamic t [Quadrant], Dynamic t (V2 Float), Width Float, Height Float) -- ^ player intersect info
  -> (Dynamic t [Quadrant], Dynamic t (V2 Float), Width Float, Height Float) -- ^ box intersect info
  -> Event t a -- ^ player interact event
  -> m (Dynamic t Bool)
mkBoxOpen playerIntersect boxIntersect ePlayerInteract = mdo
  dBoxOpen <-
    holdDyn False $
    fforMaybe
      ((,) <$>
        current dBoxOpen <*>
        current (intersects playerIntersect boxIntersect) <@ ePlayerInteract)
      (\(open, touching) ->
          if touching
          then Just $ not open
          else Nothing)
  pure dBoxOpen

mkBoxPicture
  :: Reflex t
  => (Picture, Picture)
  -> Dynamic t Bool
  -> Dynamic t Picture
mkBoxPicture (open, closed) dBoxOpen =
  (\b -> if b then open else closed) <$> dBoxOpen
