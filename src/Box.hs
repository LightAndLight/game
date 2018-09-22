{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}
{-# language RecursiveDo #-}
module Box where

import Reflex
import Control.Lens.Getter ((^.))
import Control.Monad (join)
import Control.Monad.Fix (MonadFix)
import Linear.V2 (V2)

import Dimensions (Width, Height)
import Entity (Entity, mkStaticEntity, intersects, entityQuadrants)
import Graphics.Gloss (Picture)
import Grid (Quadrant)
import GridManager.Class (GridManager)
import Map (Map)
import UniqueSupply.Class (UniqueSupply, requestUnique)
import Unique (Unique)

data Box t
  = Box
  { _boxQuadrants :: Dynamic t [Quadrant]
  , _boxOpen :: Dynamic t Bool
  , _boxPicture :: Dynamic t Picture
  , _boxPosition :: Dynamic t (V2 Float)
  , _boxWidth :: Width Float
  , _boxHeight :: Height Float
  }

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

mkBox
  :: ( Reflex t, MonadHold t m, MonadFix m
     , UniqueSupply t m, GridManager t (Entity t) m
     , Adjustable t m
     )
  => Map
  -> Event t a
  -> (Picture, Picture)
  -> Width Float
  -> Height Float
  -> V2 Float
  -> (Dynamic t [Quadrant], Dynamic t (V2 Float), Width Float, Height Float)
  -> Event t (V2 Float)
  -> m (Box t)
mkBox mp eCreate (openPic, closedPic) _boxWidth _boxHeight bPos playerIntersect ePlayerInteract = mdo
  let
    _boxPosition = pure bPos

  eUnique <- requestUnique eCreate

  _boxOpen <-
    mkBoxOpen
      playerIntersect
      (_boxQuadrants, _boxPosition, _boxWidth, _boxHeight)
      ePlayerInteract

  let _boxPicture = mkBoxPicture (openPic, closedPic) _boxOpen

  (_, eBoxEntity) <-
    runWithReplace
      (pure ())
      ((\u -> mkStaticEntity mp u _boxWidth _boxHeight bPos _boxPicture) <$>
       eUnique)

  _boxQuadrants <- join <$> holdDyn (pure []) ((^. entityQuadrants) <$> eBoxEntity)

  pure Box{..}

mkBox'
  :: ( Reflex t, MonadHold t m, MonadFix m
     , GridManager t (Entity t) m
     , Adjustable t m
     )
  => Map
  -> Unique
  -> (Picture, Picture)
  -> Width Float
  -> Height Float
  -> V2 Float
  -> (Dynamic t [Quadrant], Dynamic t (V2 Float), Width Float, Height Float)
  -> Event t (V2 Float)
  -> m (Box t)
mkBox' mp u (openPic, closedPic) _boxWidth _boxHeight bPos playerIntersect ePlayerInteract = mdo
  let _boxPosition = pure bPos

  _boxOpen <-
    mkBoxOpen
      playerIntersect
      (_boxQuadrants, _boxPosition, _boxWidth, _boxHeight)
      ePlayerInteract

  let _boxPicture = mkBoxPicture (openPic, closedPic) _boxOpen

  boxEntity <- mkStaticEntity mp u _boxWidth _boxHeight bPos _boxPicture

  let _boxQuadrants = boxEntity^.entityQuadrants

  pure Box{..}

