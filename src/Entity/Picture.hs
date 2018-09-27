{-# language FunctionalDependencies, MultiParamTypeClasses #-}
module Entity.Picture where

import Reflex (Dynamic)
import Control.Lens.Lens (Lens')
import Graphics.Gloss (Picture)

class HasPicture t e | e -> t where
  picture :: Lens' e (Dynamic t Picture)
