{-# language FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
{-# language UndecidableInstances #-}
module GridManager.Class where

import Reflex
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT)
import Data.Semigroup ((<>))
import Linear.V2 (V2)

import Dimensions (Width, Height)
import Grid (Grid(..), Quadrant(..))
import Unique (Unique)

import qualified UniqueMap

class (Reflex t, Monad m) => GridManager t g m | m -> t g where
  getGrid :: m (Grid t g)
  registerEntity
    :: Unique
    -> g
    -> (Width Float, Height Float)
    -> Event t (V2 Float)
    -> m ()

instance GridManager t g m => GridManager t g (StateT s m) where
  getGrid = lift getGrid
  registerEntity a b c d = lift $ registerEntity a b c d

instance GridManager t g m => GridManager t g (ReaderT r m) where
  getGrid = lift getGrid
  registerEntity a b c d = lift $ registerEntity a b c d

instance (Monoid w, GridManager t g m) => GridManager t g (WriterT w m) where
  getGrid = lift getGrid
  registerEntity a b c d = lift $ registerEntity a b c d

getQuadrants
  :: (MonadHold t m, MonadFix m, GridManager t g m)
  => Unique
  -> m (Dynamic t [Quadrant])
getQuadrants u = do
  grid <- getGrid

  dTl <-
    holdUniqDyn $
    isQuadrant TL u <$> _gridTopLeft grid

  dTr <-
    holdUniqDyn $
    isQuadrant TR u <$> _gridTopRight grid

  dBl <-
    holdUniqDyn $
    isQuadrant BL u <$> _gridBottomLeft grid

  dBr <-
    holdUniqDyn $
    isQuadrant BR u <$> _gridBottomRight grid

  pure $
    (\a b c d -> a <> b <> c <> d) <$>
    dTl <*>
    dTr <*>
    dBl <*>
    dBr
  where
    isQuadrant q un m =
      if UniqueMap.member un m
      then [q]
      else []
