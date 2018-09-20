{-# language DeriveFunctor, StandaloneDeriving #-}
{-# language FlexibleInstances, UndecidableInstances #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses, FunctionalDependencies #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables #-}
module Grid where

import Reflex
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State.Strict (MonadState(..), StateT)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Writer (WriterT)
import Data.Semigroup ((<>))
import Linear.V2 (V2(..))

import Dimensions (Width(..), Height(..))
import Unique (Unique)
import UniqueSupply.Class (UniqueSupply)
import UniqueMap (UniqueMap, mergeUnique)
import qualified UniqueMap

data Grid t a
  = Grid
  { _gridWidth :: Width Float -- ^ width
  , _gridHeight :: Height Float -- ^ height
  , _gridVerticalCenter :: Float -- ^ vertical center
  , _gridHorizontalCenter :: Float -- ^ horizontal center
  , _gridTopLeft :: Dynamic t (UniqueMap a) -- ^ top left
  , _gridTopRight :: Dynamic t (UniqueMap a) -- ^ top right
  , _gridBottomLeft :: Dynamic t (UniqueMap a) -- ^ bottom left
  , _gridBottomRight :: Dynamic t (UniqueMap a) -- ^ bottom right
  }
deriving instance Reflex t => Functor (Grid t)

data Quadrant = TL | TR | BL | BR deriving (Eq, Show)

