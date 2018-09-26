{-# language DeriveFunctor, StandaloneDeriving #-}
{-# language ScopedTypeVariables #-}
module Grid where

import Reflex
import Control.Lens.Getter ((^.))
import Data.Map (Map)
import Linear.V2 (V2(..), _x, _y)
import Position (HasPosition(..))

import qualified Data.Map as Map

import Dimensions (Width(..), Height(..), HasWidth(..), HasHeight(..))
import Grid.Quadrant (Quadrant(..))
import Unique (Unique)

data GridConfig
  = GridConfig
  { gcRows :: Int
  , gcColumns :: Int
  , gcWidth :: Width Float
  , gcHeight :: Height Float
  }

getQuadrants
  :: forall t m a
   . ( Reflex t, MonadHold t m, Adjustable t m
     , HasPosition t a, HasWidth a, HasHeight a
     )
  => GridConfig
  -> Event t (Map Unique (Maybe a))
  -> m (Dynamic t (Map Unique [Quadrant]))
getQuadrants (GridConfig rows cols w h) eUpdate = do
  d <- listHoldWithKey Map.empty eUpdate $ \_ a -> pure $ quadrantsFor a
  pure $ d >>= distributeMapOverDynPure
  where
    colWidth = unWidth w / fromIntegral cols
    rowHeight = unHeight h / fromIntegral rows

    quadrants :: [(V2 Float, Quadrant)]
    quadrants = do
      x <- [0..cols-1]
      y <- [0..rows-1]
      pure
        ( V2 (colWidth * fromIntegral x) (rowHeight * fromIntegral y)
        , Quadrant (x, y)
        )

    quadrantsFor
      :: (Reflex t, HasPosition t a, HasWidth a, HasHeight a)
      => a -> Dynamic t [Quadrant]
    quadrantsFor item = do
      pos <- item^.position
      pure $
        fmapMaybe
          (\(qpos, q) ->
            if
              not $
              pos^._x + unWidth (item^.width) < qpos^._x ||
              pos^._y + unHeight (item^.height) < qpos^._y ||
              pos^._x > qpos^._x + colWidth ||
              pos^._y > qpos^._y + rowHeight
            then Just q
            else Nothing)
          quadrants
