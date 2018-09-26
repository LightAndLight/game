{-# language DefaultSignatures, GADTs #-}
{-# language FunctionalDependencies, MultiParamTypeClasses #-}
module EntityStore.Class where

import Reflex
import Control.Lens.Setter (over, mapped)
import Control.Lens.Tuple (_2)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Functor.Misc (Const2(..))
import Data.Map (Map)

import Entity (Entity)
import Grid.Quadrant (Quadrant)
import Unique (Unique)

class (Reflex t, Monad m) => EntityStore t m | m -> t where
  tellEntity :: Event t (Unique, Maybe (Entity t)) -> m ()
  default tellEntity
    :: (MonadTrans u, EntityStore t n, m ~ u n)
    => Event t (Unique, Maybe (Entity t)) -> m ()
  tellEntity = lift . tellEntity

  askEntities :: m (Dynamic t (Map Unique (Entity t)))
  default askEntities
    :: (MonadTrans u, EntityStore t n, m ~ u n)
    => m (Dynamic t (Map Unique (Entity t)))
  askEntities = lift askEntities

  askQuadrants :: m (Dynamic t (Map Unique [Quadrant]))
  default askQuadrants
    :: (MonadTrans u, EntityStore t n, m ~ u n)
    => m (Dynamic t (Map Unique [Quadrant]))
  askQuadrants = lift askQuadrants

quadrantsFor
  :: ( Reflex t, MonadHold t m
     , EntityStore t m
     )
  => Event t Unique
  -> m (Dynamic t [Quadrant])
quadrantsFor eUnique = do
  eQuadrants <- updated <$> askQuadrants
  eTargetQuadrants <-
    switchHold never $ select (fanMap eQuadrants) . Const2 <$> eUnique
  holdDyn [] eTargetQuadrants
