{-# language FunctionalDependencies, MultiParamTypeClasses #-}
module Position where

import Reflex (Dynamic)
import Control.Lens.Lens (Lens')
import Linear.V2 (V2)

class HasPosition t e | e -> t where
  position :: Lens' e (Dynamic t (V2 Float))
