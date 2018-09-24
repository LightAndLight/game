{-# language FunctionalDependencies, MultiParamTypeClasses #-}
module SceneManager.Class where

import Reflex
import Data.Map (Map)
import Graphics.Gloss (Picture)

import Entity (Entity)
import Unique (Unique)

class (Reflex t, Monad m) => SceneManager t e m | m -> t e where
  getScene :: m (Dynamic t Picture)
  addToScene :: Event t (Map Unique (Maybe e)) -> m ()
