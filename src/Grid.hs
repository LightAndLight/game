{-# language DeriveFunctor, StandaloneDeriving #-}
module Grid where

import Reflex
import Control.Lens.Getter ((^.))
import Data.Map (Map)
import Linear.V2 (V2(..), _x, _y)
import Position (HasPosition(..))

import qualified Data.Map as Map

import Dimensions (Width(..), Height(..), HasWidth(..), HasHeight(..))
import Unique (Unique)

data Cell t a
  = Cell
  { cellPosition :: V2 Float
  , cellWidth :: Width Float
  , cellHeight :: Height Float
  , cellContents :: Dynamic t a
  }
deriving instance Reflex t => Functor (Cell t)

newtype Row t a
  = Row
  { unRow :: [Cell t a]
  }
deriving instance Reflex t => Functor (Row t)

newtype Rows t a
  = Rows
  { unRows :: [Row t a]
  }
deriving instance Reflex t => Functor (Rows t)

data Grid t a
  = Grid
  { _gridWidth :: Width Float -- ^ width
  , _gridHeight :: Height Float -- ^ height
  , _gridRows :: Rows t (Map Unique a)
  }
deriving instance Reflex t => Functor (Grid t)

makeGrid
  :: ( Reflex t
     , HasPosition t a, HasWidth a, HasHeight a
     )
  => Int -- ^ Number of rows
  -> Int -- ^ Numbre of columns
  -> Width Float
  -> Height Float
  -> Dynamic t (Map Unique a)
  -> Grid t a
makeGrid numRows numCols w h objects = Grid w h $ Rows cells
  where
    colWidth = Width $ unWidth w / fromIntegral numCols
    rowHeight = Height $ unHeight h / fromIntegral numRows

    xs = (unWidth colWidth *) <$> [0..fromIntegral $ numCols-1]
    ys = (unHeight rowHeight *) <$> [0..fromIntegral $ numRows-1]

    inQuadrant
      :: (Reflex t, HasPosition t a, HasWidth a, HasHeight a)
      => Float -> Float
      -> Map Unique a
      -> Dynamic t (Map Unique a)
    inQuadrant x y =
      fmap
        (Map.mapMaybe $ \(item, pos) ->
         if
           not $
           pos^._x + unWidth (item^.width) < x ||
           pos^._y + unHeight (item^.height) < y ||
           pos^._x > x + unWidth colWidth ||
           pos^._y > y + unHeight rowHeight
         then Just item
         else Nothing) .
      distributeMapOverDynPure .
      fmap (\i -> (,) i <$> i^.position)

    cells =
      (\y ->
         Row $
         (\x ->
            Cell
              (V2 x y)
              colWidth
              rowHeight
              (objects >>= inQuadrant x y)) <$>
         xs) <$>
      ys

