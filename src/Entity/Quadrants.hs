{-# language FunctionalDependencies, MultiParamTypeClasses #-}
module Entity.Quadrants where

import Reflex (Dynamic)
import Control.Lens.Lens (Lens')
import Grid.Quadrant (Quadrant)

class HasQuadrants t e | e -> t where
  quadrants :: Lens' e (Dynamic t [Quadrant])
