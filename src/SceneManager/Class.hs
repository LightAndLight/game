{-# language FunctionalDependencies, MultiParamTypeClasses #-}
module SceneManager.Class where

import Reflex
import Data.Map (Map, singleton)
import Graphics.Gloss (Picture)

import Entity (Entity)
import Unique (Unique)

class (Reflex t, Monad m) => SceneManager t m | m -> t where
  getScene :: m (Dynamic t Picture)
  addToScene :: Event t (Map Unique (Maybe (Entity t))) -> m ()
