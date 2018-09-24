{-# language FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables #-}
module GridManager.Base where

import Reflex
import Reflex.NotReady.Class (NotReady(..))
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State (MonadState(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Map (Map)

import qualified Data.Map as Map

import Dimensions (Width(..), Height(..), HasWidth, HasHeight)
import Grid (Grid(..), makeGrid)
import GridManager.Class (GridManager(..))
import Position (HasPosition)
import RandomGen.Class (RandomGen(..))
import SceneManager.Class (SceneManager(..))
import Unique (Unique)
import UniqueSupply.Class (UniqueSupply(..))

import qualified Map as Game

newtype GridManagerT t g m a
  = GridManagerT
  { unGridManagerT
    :: ReaderT
         (Grid t g)
         (DynamicWriterT t (Map Unique g) m)
         a
  } deriving
  ( Functor, Applicative, Monad, MonadFix
  , MonadSample t, MonadHold t, PostBuild t
  )

instance (Adjustable t m, MonadHold t m, MonadFix m) => Adjustable t (GridManagerT t g m) where
  runWithReplace a b = GridManagerT $ runWithReplace (unGridManagerT a) (unGridManagerT <$> b)
  traverseIntMapWithKeyWithAdjust a b c =
    GridManagerT $
    traverseIntMapWithKeyWithAdjust
      (\x y -> unGridManagerT $ a x y)
      b
      c
  traverseDMapWithKeyWithAdjust a b c =
    GridManagerT $
    traverseDMapWithKeyWithAdjust
      (\x y -> unGridManagerT $ a x y)
      b
      c
  traverseDMapWithKeyWithAdjustWithMove a b c =
    GridManagerT $
    traverseDMapWithKeyWithAdjustWithMove
      (\x y -> unGridManagerT $ a x y)
      b
      c

runGridManagerT
  :: forall t g m a
   . ( Reflex t, MonadHold t m, MonadFix m
     , HasPosition t g, HasWidth g, HasHeight g
     )
  => Int -- ^ Number of rows
  -> Int -- ^ Number of columns
  -> Game.Map
  -> GridManagerT t g m a
  -> m a
runGridManagerT rows cols mp (GridManagerT m) = mdo
  let
    grid :: Grid t g
    grid =
      makeGrid
        rows cols
        (Game._mapWidth mp) (Game._mapHeight mp)
        dItems

  (a, dItems) <- runDynamicWriterT $ runReaderT m grid

  pure a

instance MonadTrans (GridManagerT t g) where
  lift = GridManagerT . lift . lift

instance (Reflex t, Monad m) => GridManager t g (GridManagerT t g m) where
  getGrid = GridManagerT ask
  registerEntity = registerEntityImpl

instance UniqueSupply t m => UniqueSupply t (GridManagerT t g m) where
  requestUnique = lift . requestUnique

instance RandomGen t m => RandomGen t (GridManagerT t g m) where
  randomInt = lift . randomInt
  randomIntR = lift . randomIntR

instance MonadState s m => MonadState s (GridManagerT t g m) where
  get = lift get
  put = lift . put

instance NotReady t m => NotReady t (GridManagerT t g m) where
  notReadyUntil = lift . notReadyUntil
  notReady = lift notReady

instance SceneManager t m => SceneManager t (GridManagerT t g m) where
  getScene = lift getScene
  addToScene = lift . addToScene

registerEntityImpl
  :: (Reflex t, Monad m)
  => Unique
  -> g
  -> GridManagerT t g m ()
registerEntityImpl a b =
  GridManagerT $ tellDyn . pure $ Map.singleton a b
