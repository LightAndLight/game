{-# language FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language UndecidableInstances #-}
module GridManager.Class where

import Reflex
import Control.Lens.Setter (over, mapped)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT)
import Data.List (findIndices)

import qualified Data.Map as Map

import Grid (Grid(..), Rows(..), Row(..), Cell(..))
import Grid.Quadrant (Quadrant(..))
import Unique (Unique)

class (Reflex t, Monad m) => GridManager t g m | m -> t g where
  getGrid :: m (Grid t g)
  registerEntity :: Unique -> g -> m ()

instance GridManager t g m => GridManager t g (StateT s m) where
  getGrid = lift getGrid
  registerEntity a b = lift $ registerEntity a b

instance GridManager t g m => GridManager t g (ReaderT r m) where
  getGrid = lift getGrid
  registerEntity a b = lift $ registerEntity a b

instance (Monoid w, GridManager t g m) => GridManager t g (WriterT w m) where
  getGrid = lift getGrid
  registerEntity a b = lift $ registerEntity a b

getQuadrants
  :: forall t g m
   . (MonadHold t m, MonadFix m, GridManager t g m)
  => Unique
  -> m (Dynamic t [Quadrant])
getQuadrants u = do
  grid <- getGrid

  let
    rows :: Dynamic t [[Bool]]
    rows =
      let
        g = fmap unRow . unRows . _gridRows $ grid
        g' = over (mapped.mapped) (fmap (Map.member u) . cellContents) g
      in
        distributeListOverDynPure $ distributeListOverDynPure <$> g'

  pure $
    fmap
      (\rs -> do
          (y, row) <- zip [0::Int ..] rs
          x <- findIndices id row
          pure $ Quadrant (x, y))
      rows
