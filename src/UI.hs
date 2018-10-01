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
import Control.Lens.Setter ((.~), (-~), (+~), (%~), mapped)
import Control.Lens.TH (makeLenses)
import Control.Lens.Tuple (_1, _2, _3)
import Control.Lens.Wrapped (_Wrapped)
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

data RElement t
  = RElement
  { _relWidth :: Width Float
  , _relHeight :: Height Float
  , _relPicture :: Dynamic t Picture
  , _relMouseInside
    :: Dynamic t (V3 Float -> Width Float -> Height Float -> Bool)
  }
makeLenses ''RElement

toElement :: Reflex t => Dynamic t (V3 Float) -> RElement t -> Element t
toElement dPos (RElement w h p mi) =
  Element dPos w h p $ mi <*> dPos <*> pure w <*> pure h

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

mouseInside
  :: (MonadHold t m, MonadFix m, UIBuilder t m)
  => m (Dynamic t (V3 Float -> Width Float -> Height Float -> Bool))
mouseInside = do
  events <- askInputs
  Viewport{..} <- askViewport
  let
    eMove =
      (\(x, y) -> (x + (unWidth _vpWidth/2), -y + (unHeight _vpHeight/2))) <$>
      select events GE_Motion
  holdDyn
    (\_ _ _ -> False)
    ((\(cx, cy) (V3 elx ely _) w h ->
      elx <= cx && cx < elx + unWidth w &&
      ely <= cy && cy < ely + unHeight h) <$>
    eMove)

text
  :: forall t m
   . (MonadHold t m, MonadFix m, UIBuilder t m)
  => String
  -> m (RElement t)
text str = do
  font <- askFont
  let
    ls = toLetters font str
    lsW = fromIntegral $ lettersWidth ls
    lsH = fromIntegral $ lettersHeight ls
    w = Width lsW
    h = Height lsH
  dMouseInside <- mouseInside
  pure $ 
    RElement
    { _relWidth = w
    , _relHeight = h
    , _relPicture = pure $ translate (lsW / 2) (-lsH / 2) (drawLetters ls)
    , _relMouseInside = dMouseInside
    }

margin
  :: Reflex t
  => Float -- ^ top margin
  -> Float -- ^ bottom margin
  -> Float -- ^ left margin
  -> Float -- ^ right margin
  -> RElement t
  -> RElement t
margin t b l r el =
  el &
    relWidth._Wrapped +~ (l+r) &
    relHeight._Wrapped +~ (t+b) &
    relPicture.mapped %~ translate l (-t)

bordered
  :: Reflex t
  => Float -- ^ border thickness
  -> RElement t
  -> RElement t
bordered thickness el =
  el &
    relWidth._Wrapped +~ (2*thickness) &
    relHeight._Wrapped +~ (2*thickness) &
    relPicture .~
      ((\pic ->
          translate thickness (-thickness) $
          pic <>
          foldMap
            (\n ->
                lineLoop
                [ (n - thickness, -n + thickness)
                , (-n + w + thickness, -n + thickness)
                , (-n + w + thickness, n - h - thickness)
                , (n - thickness, n - h - thickness)
                ])
            [0..thickness-1]) <$>
      _relPicture el)
  where
    w = unWidth $ _relWidth el
    h = unHeight $ _relHeight el

clicked
  :: (MonadHold t m, MonadFix m, UIBuilder t m)
  => RElement t
  -> m (Dynamic t (V3 Float) -> Width Float -> Height Float -> Event t ())
clicked el = do
  events <- askInputs
  let
    eClick =
      select events $
      GE_Key (Just $ MouseButton LeftButton) (Just Down) Nothing
  pure $
    \dPos w h ->
      void $
      ffilter
        id
        (current (_relMouseInside el) <*>
         current dPos <*>
         pure w <*>
         pure h <@
         eClick)

mouseEntered
  :: (MonadHold t m, MonadFix m, UIBuilder t m)
  => RElement t
  -> m (Dynamic t (V3 Float) -> Width Float -> Height Float -> Event t ())
mouseEntered el = do
  let
    dInElement = _relMouseInside el
    eEntered =
      (\fx fy p w h -> if not (fx p w h) && fy p w h then Just () else Nothing) <$>
      current dInElement <@>
      updated dInElement
  pure $
    \dPos w h ->
      fmapMaybe id ((\a f -> f a w h) <$> current dPos <@> eEntered)

mouseLeft
  :: (MonadHold t m, MonadFix m, UIBuilder t m)
  => RElement t
  -> m (Dynamic t (V3 Float) -> Width Float -> Height Float -> Event t ())
mouseLeft el = do
  let
    dInElement = _relMouseInside el
    eLeft =
      (\fx fy p w h -> if fx p w h && not (fy p w h) then Just () else Nothing) <$>
      current dInElement <@>
      updated dInElement
  pure $
    \dPos w h -> fmapMaybe id ((\a f -> f a w h) <$> current dPos <@> eLeft)

button
  :: (MonadHold t m, MonadFix m, UIBuilder t m)
  => Dynamic t (V3 Float)
  -> String
  -> m (Event t (), Element t)
button dPosition msg = do
  rel <- bordered 2 . margin 5 5 5 5 <$> text msg
  feEntered <- mouseEntered rel
  feLeft <- mouseLeft rel
  feClicked <- clicked rel
  let
    w = unWidth $ _relWidth rel
    h = unHeight $ _relHeight rel
    dPictureUnhighlighed = _relPicture rel
    dPictureHighlighed =
      (color
         (greyN 0.7)
         (polygon [(0, 0), (w, 0), (w, -h), (0, -h)]) <>) <$> _relPicture rel
  dPicture <-
    join <$>
    holdDyn
      dPictureUnhighlighed
      (leftmost
       [ dPictureHighlighed <$
         feEntered dPosition (_relWidth rel) (_relHeight rel)
       , dPictureUnhighlighed <$
         feLeft dPosition (_relWidth rel) (_relHeight rel)
       ])
  pure
    ( feClicked dPosition (_relWidth rel) (_relHeight rel)
    , toElement dPosition rel & elPicture .~ dPicture
    )
