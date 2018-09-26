module Render where

import Reflex
import Data.Map (Map)
import Graphics.Gloss (Picture)

import Entity (Entity)
import Render.Entity (renderedEntity)
import Unique (Unique)
import Viewport (Viewport)

render
  :: Reflex t
  => Viewport t
  -> Dynamic t (Map Unique (Entity t))
  -> Dynamic t Picture
render vp d = d >>= foldMap (renderedEntity vp)
