{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language RecordWildCards #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables #-}
module EntityStore.Base where

import Reflex
import Reflex.NotReady.Class (NotReady)
import Data.Map (Map)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Monad.Trans.Class (MonadTrans(..))

import qualified Data.Map as Map

import Entity (Entity(..))
import EntityStore.Class (EntityStore(..))
import Grid (GridConfig, getQuadrants)
import Grid.Quadrant (Quadrant)
import RandomGen.Class (RandomGen)
import Unique (Unique)
import UniqueSupply.Class (UniqueSupply)

data EntityData t
  = EntityData
  { _edEntities :: Dynamic t (Map Unique (Entity t))
  , _edQuadrants :: Dynamic t (Map Unique [Quadrant])
  }

newtype EntityStoreT t m a
  = EntityStoreT
  { unEntityStoreT
    :: ReaderT
         (EntityData t)
         (EventWriterT t (Map Unique (Maybe (Entity t))) m)
         a
  } deriving
  ( Functor, Applicative, Monad, MonadFix
  , MonadSample t, MonadHold t, PostBuild t
  , UniqueSupply t, RandomGen t, NotReady t
  )

instance MonadTrans (EntityStoreT t) where
  lift = EntityStoreT . lift . lift

runEntityStoreT
  :: forall t m a
    . ( Reflex t, MonadHold t m, MonadFix m
      , Adjustable t m, NotReady t m, PostBuild t m
      )
  => GridConfig -> EntityStoreT t m a -> m a
runEntityStoreT gc (EntityStoreT m) = do
  rec
    (a, eUpdate) <- runEventWriterT $ runReaderT m EntityData{..}
    _edEntities <- listHoldWithKey Map.empty eUpdate (\_ -> pure)
    _edQuadrants <- getQuadrants gc eUpdate
  pure a

instance (Reflex t, Monad m) => EntityStore t (EntityStoreT t m) where
  askEntities = EntityStoreT $ asks _edEntities
  askQuadrants = EntityStoreT $ asks _edQuadrants
  tellEntity = EntityStoreT . tellEvent . fmap (uncurry Map.singleton)

instance (Adjustable t m, MonadHold t m, MonadFix m) => Adjustable t (EntityStoreT t m) where
  runWithReplace a b = EntityStoreT $ runWithReplace (unEntityStoreT a) (unEntityStoreT <$> b)
  traverseIntMapWithKeyWithAdjust a b c =
    EntityStoreT $
    traverseIntMapWithKeyWithAdjust
      (\x y -> unEntityStoreT $ a x y)
      b
      c
  traverseDMapWithKeyWithAdjust a b c =
    EntityStoreT $
    traverseDMapWithKeyWithAdjust
      (\x y -> unEntityStoreT $ a x y)
      b
      c
  traverseDMapWithKeyWithAdjustWithMove a b c =
    EntityStoreT $
    traverseDMapWithKeyWithAdjustWithMove
      (\x y -> unEntityStoreT $ a x y)
      b
      c
