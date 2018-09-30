{-# language ConstraintKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies, MultiParamTypeClasses #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language UndecidableInstances #-}
module UI where

import Reflex
import Reflex.Gloss.Event
  (GlossEvent(..), Key(..), MouseButton(..), KeyState(..))
import Control.Lens.Getter ((^.), view)
import Control.Lens.Setter ((.~))
import Control.Lens.TH (makeLenses)
import Control.Lens.Tuple (_1, _2, _3)
import Control.Monad (join, void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (MonadReader, ask)
import Data.Function ((&))
import Data.Semigroup ((<>))
import Graphics.Gloss (Picture, blank, color, polygon, lineLoop, translate)
import Graphics.Gloss.Data.Color (greyN)
import Linear.V3 (V3(..), _x, _y)

import Dimensions (Width(..), Height(..))
import Font (Font, drawLetters, toLetters, lettersWidth, lettersHeight)
import Viewport (Viewport(..))

data Element t
  = Element
  { _elPosition :: Dynamic t (V3 Float)
  , _elWidth :: Width Float
  , _elHeight :: Height Float
  , _elPicture :: Dynamic t Picture
  , _elMouseInside :: Dynamic t Bool
  }
makeLenses ''Element

renderElement
  :: Reflex t
  => Viewport t
  -> Element t
  -> Dynamic t Picture
renderElement Viewport{..} el =
  (\pos ->
     translate
       (pos^._x - (unWidth _vpWidth / 2))
       (-pos^._y + (unHeight _vpHeight / 2))) <$>
  _elPosition el <*>
  _elPicture el

origin :: Reflex t => Element t
origin =
  Element (pure 0) (Width 0) (Height 0) (pure blank) (pure False)

class
  ( Reflex t
  , MonadReader (EventSelector t GlossEvent, Font, Viewport t) m
  ) =>
  UIBuilder t m where

instance
  ( Reflex t
  , MonadReader (EventSelector t GlossEvent, Font, Viewport t) m
  ) =>
  UIBuilder t m where

askInputs :: UIBuilder t m => m (EventSelector t GlossEvent)
askInputs = view _1 <$> ask

askFont :: UIBuilder t m => m Font
askFont = view _2 <$> ask

askViewport :: UIBuilder t m => m (Viewport t)
askViewport = view _3 <$> ask

text
  :: forall t m
   . (MonadHold t m, MonadFix m, UIBuilder t m)
  => Dynamic t (V3 Float)
  -> String
  -> m (Element t)
text dPos str = do
  font <- askFont
  let
    ls = toLetters font str
    lsW = fromIntegral $ lettersWidth ls
    lsH = fromIntegral $ lettersHeight ls
    w = Width lsW
    h = Height lsH
  dMouseInside <- mouseInside dPos w h
  pure $ 
    Element
    { _elPosition = dPos
    , _elWidth = w
    , _elHeight = h
    , _elPicture = pure $ translate (lsW / 2) (-lsH / 2) (drawLetters ls)
    , _elMouseInside = dMouseInside
    }

bordered
  :: Reflex t
  => Element t
  -> Element t
bordered el =
    el &
    elPicture .~
      ((\pic ->
          pic <>
          lineLoop [(0, 0), (w, 0), (w, -h), (0, -h)]) <$>
      _elPicture el)
  where
    w = unWidth $ _elWidth el
    h = unHeight $ _elHeight el

mouseInside
  :: (MonadHold t m, MonadFix m, UIBuilder t m)
  => Dynamic t (V3 Float)
  -> Width Float
  -> Height Float
  -> m (Dynamic t Bool)
mouseInside dPosition w h = do
  events <- askInputs
  Viewport{..} <- askViewport
  let
    eMove =
      (\(x, y) -> (x + (unWidth _vpWidth/2), -y + (unHeight _vpHeight/2))) <$>
      select events GE_Motion
  holdUniqDyn =<<
    holdDyn
      False
      ((\(V3 elx ely _) (cx, cy) ->
        elx <= cx && cx < elx + unWidth w &&
        ely <= cy && cy < ely + unHeight h) <$>
      current dPosition <@>
      eMove)

clicked
  :: (MonadHold t m, MonadFix m, UIBuilder t m)
  => Element t
  -> m (Event t ())
clicked el = do
  events <- askInputs
  let
    eClick =
      select events $
      GE_Key (Just $ MouseButton LeftButton) (Just Down) Nothing
  pure . void $ ffilter id (current (_elMouseInside el) <@ eClick)

mouseEntered
  :: (MonadHold t m, MonadFix m, UIBuilder t m)
  => Element t
  -> m (Event t ())
mouseEntered el = do
  let
    dInElement = _elMouseInside el
    eEntered =
      attachWithMaybe
        (\x y -> if not x && y then Just () else Nothing)
        (current dInElement)
        (updated dInElement)
  pure eEntered

mouseLeft
  :: (MonadHold t m, MonadFix m, UIBuilder t m)
  => Element t
  -> m (Event t ())
mouseLeft el = do
  let
    dInElement = _elMouseInside el
    eLeft =
      attachWithMaybe
        (\x y -> if x && not y then Just () else Nothing)
        (current dInElement)
        (updated dInElement)
  pure eLeft

button
  :: (MonadHold t m, MonadFix m, UIBuilder t m)
  => Dynamic t (V3 Float)
  -> String
  -> m (Event t (), Element t)
button dPosition msg = do
  el <- bordered <$> text dPosition msg
  eEntered <- mouseEntered el
  eLeft <- mouseLeft el
  eClicked <- clicked el
  let
    w = unWidth $ _elWidth el
    h = unHeight $ _elHeight el
    dPictureUnhighlighed = _elPicture el
    dPictureHighlighed =
      (color
         (greyN 0.7)
         (polygon [(0, 0), (w, 0), (w, -h), (0, -h)]) <>) <$> _elPicture el
  dPicture <-
    join <$>
    holdDyn
      dPictureUnhighlighed
      (leftmost
       [ dPictureHighlighed <$ eEntered
       , dPictureUnhighlighed <$ eLeft
       ])
  pure (eClicked, el & elPicture .~ dPicture)
