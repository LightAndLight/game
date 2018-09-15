{-# language DeriveFunctor, StandaloneDeriving #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses, FunctionalDependencies #-}
module Grid where

import Control.Lens.Lens (Lens')
import Control.Lens.Setter (over, mapped)
import Control.Lens.Tuple (_2)
import Data.Semigroup ((<>))
import Linear.V2 (V2(..))
import Reflex (Reflex, Dynamic, Event, MonadHold, holdDyn)

class HasGrid s t a | s -> t a where
  grid :: Lens' s (Grid t a)

data Grid t a
  = Grid
  { _gridWidth :: Float -- ^ width
  , _gridHeight :: Float -- ^ height
  , _gridVerticalCenter :: Float -- ^ vertical center
  , _gridHorizontalCenter :: Float -- ^ horizontal center
  , _gridTopLeft :: Dynamic t [a] -- ^ top left
  , _gridTopRight :: Dynamic t [a] -- ^ top right
  , _gridBottomLeft :: Dynamic t [a] -- ^ bottom left
  , _gridBottomRight :: Dynamic t [a] -- ^ bottom right
  }
deriving instance Reflex t => Functor (Grid t)

emptyGrid
  :: Reflex t
  => (Float, Float) -- ^ the grid's (width, height, vertical centerline, horizontal centerline)
  -> Grid t a
emptyGrid (gW, gH) | gVC <- gW / 2, gHC <- gH / 2 =
  Grid gW gH gVC gHC (pure []) (pure []) (pure []) (pure [])

gridOf
  :: (Reflex t, MonadHold t m)
  => (Float, Float) -- ^ the grid's (width, height, vertical centerline, horizontal centerline)
  -> [(a, (Float, Float), (Float, Float), Event t (V2 Float))] -- ^ [(item, item's initial position, item moved to these coordinates)]
  -> m (Grid t a, [Dynamic t [Quadrant]])
gridOf gDims = go $ emptyGrid gDims
  where
    go grd [] = pure (grd, [])
    go grd ((a, aPos, aDims, eMoved) : rest) = do
      (grd', qdrs) <- insertG a aPos aDims eMoved grd
      over (mapped._2) (qdrs :) $ go grd' rest

data Quadrant = TL | TR | BL | BR deriving (Eq, Show)

-- | insert an element into the grid
--
-- if the bounding box crosses into other parts of the grid
-- then the element is inserted into those parts too
insertG
  :: (Reflex t, MonadHold t m)
  => a -- ^ the item
  -> (Float, Float) -- ^ the items' intitial (x coordinate, y coordinate)
  -> (Float, Float) -- ^ the item's (width, height)
  -> Event t (V2 Float) -- ^ the item moved to these coordinates
  -> Grid t a
  -> m (Grid t a, Dynamic t [Quadrant])
insertG a (aX, aY) (aW, aH) eMovedTo (Grid gW gH gVC gHC tl tr bl br) = do
  let
    eBoundsChecked = boundsChecked <$> eMovedTo
    initial = boundsChecked $ V2 aX aY

  tl' <- holdDyn (tlf initial) (tlf <$> eBoundsChecked)
  tr' <- holdDyn (trf initial) (trf <$> eBoundsChecked)
  bl' <- holdDyn (blf initial) (blf <$> eBoundsChecked)
  br' <- holdDyn (brf initial) (brf <$> eBoundsChecked)

  pure
    ( Grid gW gH gVC gHC
        (fst <$> tl' <*> tl)
        (fst <$> tr' <*> tr)
        (fst <$> bl' <*> bl)
        (fst <$> br' <*> br)
    , (\q1 q2 q3 q4 -> q1 <> q2 <> q3 <> q4) <$>
      fmap snd tl' <*>
      fmap snd tr' <*>
      fmap snd bl' <*>
      fmap snd br'
    )
  where
    boundsChecked (V2 aX aY) = (aX < gVC, aY < gHC, aX + aW < gVC, aY + aH < gHC)
    tlf =
      \case
         -- top left
         (True, True, False, False) -> -- leaks into right half and bottom half
           ((a :), [TL])
         (True, True, False, True) -> -- leaks into right half but not bottom half
           ((a :), [TL])
         (True, True, True, False) -> -- leaks into bottom half but not right half
           ((a :), [TL])
         (True, True, True, True) -> -- leaks into neither right half not bottom half
           ((a :), [TL])
         _ -> (id, [])

    trf =
      \case
          -- top left
          (True, True, False, False) -> -- leaks into right half and bottom half
            ((a :), [TR])
          (True, True, False, True) -> -- leaks into right half but not bottom half
            ((a :), [TR])
          -- top right
          (False, True, _, False) -> -- leaks into bottom half
            ((a :), [TR])
          (False, True, _, True) -> -- doesn't leak into bottom half
            ((a :), [TR])
          _ -> (id, [])

    blf =
       \case
          -- top left
          (True, True, False, False) -> -- leaks into right half and bottom half
            ((a :), [BL])
          (True, True, True, False) -> -- leaks into bottom half but not right half
            ((a :), [BL])
          -- bottom left
          (True, False, False, _) -> -- leaks into right half
            ((a :), [BL])
          (True, False, True, _) -> -- doesn't leak into right half
            ((a :), [BL])
          _ -> (id, [])

    brf =
      \case
          -- top left
          (True, True, False, False) -> -- leaks into right half and bottom half
            ((a :), [BR])
          -- bottom left
          (True, False, False, _) -> -- leaks into right half
            ((a :), [BR])
          -- top right
          (False, True, _, False) -> -- leaks into bottom half
            ((a :), [BR])
          -- bottom right
          (False, False, _, _) ->
            ((a :), [BR])
          _ -> (id, [])

-- | get the elements of the quadrant that contains the point
lookupG
  :: Float -- ^ x coordinate
  -> Float -- ^ y coordinate
  -> Grid t a
  -> Dynamic t [a]
lookupG x y (Grid _ _ gVC gHC tl tr bl br) =
  case (x < gVC, y < gHC) of
    (True, True) -> tl
    (True, False) -> bl
    (False, True) -> bl
    (False, False) -> br
