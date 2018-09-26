{-# language DefaultSignatures, GADTs #-}
{-# language FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
{-# language UndecidableInstances #-}
module UniqueSupply.Class where

import Reflex
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Writer (WriterT)

import Unique

class (Reflex t, Monad m) => UniqueSupply t m | m -> t where
  requestUnique :: Event t x -> m (Event t Unique)
  default requestUnique
    :: (MonadTrans u, UniqueSupply t n, m ~ u n)
    => Event t x ->
    m (Event t Unique)
  requestUnique = lift . requestUnique

instance UniqueSupply t m => UniqueSupply t (StateT s m)
instance (UniqueSupply t m, Monoid w) => UniqueSupply t (WriterT w m)
instance UniqueSupply t m => UniqueSupply t (ReaderT e m)
instance UniqueSupply t m => UniqueSupply t (EventWriterT t w m)
