{-# language GeneralizedNewtypeDeriving #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language FunctionalDependencies, MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
module Action where

import Reflex
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.List.NonEmpty (NonEmpty)

import Entity (Entity)
import Unique (Unique)

data Action t m
  = CreateEntity (Event t Unique -> m (Entity t))
  | DeleteEntity Unique

newtype ActionT t m a
  = ActionT
  { unActionT :: EventWriterT t (NonEmpty (Action t m)) m a
  } deriving
  ( Functor, Applicative, Monad, MonadFix
  )

runActionT
  :: (Reflex t, Monad m)
  => ActionT t m a
  -> m (a, Event t (NonEmpty (Action t m)))
runActionT (ActionT m) = runEventWriterT m

instance MonadTrans (ActionT t) where
  lift = ActionT . lift

class (Reflex t, Monad m, Monad (ActionM m)) => MonadAction t m | m -> t where
  type ActionM m :: * -> *
  tellAction :: Monad m => Event t (Action t (ActionM m)) -> m ()

instance (Reflex t, Monad m) => MonadAction t (ActionT t m) where
  type ActionM (ActionT t m) = m
  tellAction e = ActionT $ tellEvent (pure <$> e)
