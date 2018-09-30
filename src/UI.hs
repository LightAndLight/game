{-# language FlexibleContexts #-}
{-# language FunctionalDependencies, MultiParamTypeClasses #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies #-}
module UI where

import Reflex
import Reflex.Gloss.Event (GlossEvent)
import Control.Lens.Setter ((.~))
import Control.Lens.TH (makeLenses)
import Graphics.Gloss (Picture, blank, text, scale)
import Linear.V3 (V3(..))
import Linear.Vector ((*^))

import Dimensions (Width(..), Height(..))

data Element t n
  = Element
  { _elPosition :: V3 n
  , _elWidth :: Width n
  , _elHeight :: Height n
  , _elPicture :: Dynamic t Picture
  }
makeLenses ''Element

-- |
-- a basis for the R^3 vector space
--
-- origin = 0, 0, 0 = top left of screen
--
-- +x is to the right
-- +y is down
-- +z is into the screen
data Direction
  = Above
  | Below
  | LeftOf
  | RightOf
  | Behind
  | InFrontOf
  deriving (Eq, Show)

origin :: (Reflex t, Num n) => Element t n
origin = Element (V3 0 0 0) (Width 0) (Height 0) (pure blank)

toBasis :: Num n => Direction -> V3 n
toBasis Above = V3 0 (-1) 0
toBasis Below = V3 0 1 0
toBasis LeftOf = V3 (-1) 0 0
toBasis RightOf = V3 1 0 0
toBasis Behind = V3 0 0 1
toBasis InFrontOf = V3 0 0 (-1)

class Num (Field a) => Positioned a where
  type Field a
  absolute :: V3 (Field a) -> a -> a

relative
  :: Positioned a
  => (Field a, Direction)
  -> V3 (Field a)
  -> a
  -> a
relative (n, dir) pos f = absolute (pos + v) f
  where
    v = n *^ toBasis dir

instance Num n => Positioned (Element t n) where
  type Field (Element t n) = n

  absolute p = elPosition .~ p

class UIBuilder t m | m -> t where
  askEvents :: m (EventSelector t GlossEvent)
  tellElement :: Element t Float -> m ()

data GlossText
  = GlossText
  { _gtSize :: Float
  , _gtValue :: String
  }

glossText :: String -> GlossText
glossText = GlossText 1.0

glossTextPicture :: GlossText -> Picture
glossTextPicture (GlossText sz val) =
  scale (0.15 * sz) (0.15 * sz) $ text val

button :: UIBuilder t m => String -> m (Event t ())
button msg = undefined
