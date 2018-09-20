{-# language FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language RecursiveDo #-}
module Unique where

import Reflex
import Control.Concurrent.Supply (Supply, newSupply, freshId)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State.Strict
  (MonadState(..), StateT, evalState, gets)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Writer (WriterT)
import Data.Functor.Const (Const(..))

newtype Unique = Unique { unUnique :: Int }
  deriving (Eq, Ord, Show)

unsafeMkUnique :: Int -> Unique
unsafeMkUnique = Unique
