{-# language FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
{-# language UndecidableInstances #-}
module UniqueSupply.Class where

import Reflex
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT)

import Unique

class (Reflex t, Monad m) => UniqueSupply t m | m -> t where
  requestUnique :: Event t x -> m (Event t Unique)

instance UniqueSupply t m => UniqueSupply t (StateT s m) where
  requestUnique = lift . requestUnique

instance (UniqueSupply t m, Monoid w) => UniqueSupply t (WriterT w m) where
  requestUnique = lift . requestUnique

instance UniqueSupply t m => UniqueSupply t (ReaderT e m) where
  requestUnique = lift . requestUnique
