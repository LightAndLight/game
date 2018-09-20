{-# language FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
{-# language UndecidableInstances #-}
module RandomGen.Class where

import Reflex
import Control.Monad.Reader (MonadReader(..), ReaderT)
import Control.Monad.State (MonadState(..), StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (MonadWriter(..), WriterT)

class (Reflex t, Monad m) => RandomGen t m | m -> t where
  randomInt :: Event t x -> m (Event t Int)

instance RandomGen t m => RandomGen t (StateT s m) where
  randomInt = lift . randomInt

instance (RandomGen t m, Monoid w) => RandomGen t (WriterT w m) where
  randomInt = lift . randomInt

instance RandomGen t m => RandomGen t (ReaderT e m) where
  randomInt = lift . randomInt
