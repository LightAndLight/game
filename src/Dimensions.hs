{-# language RoleAnnotations #-}
{-# language TemplateHaskell #-}
{-# language FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
module Dimensions where

import Control.Lens.Lens (Lens')
import Control.Lens.TH (makeWrapped)

newtype Width a = Width { unWidth :: a }
  deriving (Eq, Show)
type role Width nominal

newtype Height a = Height { unHeight :: a }
  deriving (Eq, Show)
type role Height nominal

class HasWidth e where
  width :: Lens' e (Width Float)

class HasHeight e where
  height :: Lens' e (Height Float)

makeWrapped ''Width
makeWrapped ''Height
