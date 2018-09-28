{-# language FlexibleContexts #-}
{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language RecordWildCards #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language TupleSections #-}
module Box where

import Reflex
import Reflex.Network (networkView)
import Reflex.NotReady.Class (NotReady)
import Control.Lens.Fold (foldMapOf, folded)
import Control.Lens.Getter (view)
import Control.Lens.Operators ((<&>))
import Control.Lens.TH (makeLenses)
import Control.Monad.Fix (MonadFix)
import Data.Semigroup ((<>))
import Linear.V2 (V2(..))

import qualified Data.Map

import Dimensions (Width(..), Height(..), HasWidth(..), HasHeight(..))
import Entity.Init (mkPosNotOnWith)
import Entity.Intersects (intersects)
import Entity.Picture (HasPicture(..))
import Entity.Position (HasPosition(..), mkEntityPosition)
import Entity.Quadrants (HasQuadrants(..))
import Graphics.Gloss (Picture)
import Grid (GridConfig(..), getQuadrants'')
import Grid.Quadrant (Quadrant)
import Map (Map(..))
import Player (Player(..))
import RandomGen.Class (RandomGen)
import Unique (Unique)
import UniqueSupply.Class (UniqueSupply, requestUnique)

data Box t
  = Box
  { _boxQuadrants :: Dynamic t [Quadrant]
  , _boxOpen :: Dynamic t Bool
  , _boxOpenedFirstTime :: Event t ()
  , _boxPicture :: Dynamic t Picture
  , _boxPosition :: Dynamic t (V2 Float)
  , _boxWidth :: Width Float
  , _boxHeight :: Height Float
  , _boxUpdate :: Event t (Data.Map.Map Unique (Maybe (Box t)))
  }
makeLenses ''Box

instance HasQuadrants t (Box t) where; quadrants = boxQuadrants
instance HasPosition t (Box t) where; position = boxPosition
instance HasPicture t (Box t) where; picture = boxPicture
instance HasWidth (Box t) where; width = boxWidth
instance HasHeight (Box t) where; height = boxHeight

mkBoxOpen
  :: (Reflex t, MonadHold t m, MonadFix m)
  => Player t
  -> Box t
  -> m (Dynamic t Bool)
mkBoxOpen player box = mdo
  dBoxOpen <-
    holdDyn False $
    fforMaybe
      ((,) <$>
        current dBoxOpen <*>
        current (intersects player box) <@ _playerInteract player)
      (\(open, touching) ->
          if touching
          then Just $ not open
          else Nothing)
  pure dBoxOpen

mkBoxPicture
  :: Reflex t
  => (Picture, Picture)
  -> Dynamic t Bool
  -> Dynamic t Picture
mkBoxPicture (open, closed) dBoxOpen =
  (\b -> if b then open else closed) <$> dBoxOpen

mkBoxUpdate
  :: forall t m
   . ( Reflex t, MonadHold t m, MonadFix m
     , UniqueSupply t m, RandomGen t m, Adjustable t m
     , NotReady t m, PostBuild t m
     )
  => Map
  -> GridConfig
  -> Unique
  -> Player t
  -> (Picture, Picture)
  -> Event t ()
  -> Event t ()
  -> m (Event t (Data.Map.Map Unique (Maybe (Box t))))
mkBoxUpdate mp gc u player pic openedFirstTime openedFiveTimes = do
  eUnique <- requestUnique openedFirstTime
  eMkBox :: Event t (m (Data.Map.Map Unique (Maybe (Box t)))) <-
    mkPosNotOnWith
      (0, 990)
      (0, 990)
      (_playerPosition player)
      eUnique
      (\newU pos ->
         Data.Map.singleton newU . Just <$>
         mkBox mp gc newU pic (Width 10) (Height 10) pos player)

  eCreate <- networkView =<< holdDyn (pure mempty) eMkBox
  let eDelete = Data.Map.singleton u Nothing <$ openedFiveTimes
  pure $
    eDelete <>
    eCreate

-- | initialise a single box. the returned event is the creation event for that box.
-- for more dynamic applciations, use 'mkBox'
initBox
  :: ( Reflex t, MonadHold t m, MonadFix m
     , UniqueSupply t m, RandomGen t m
     , Adjustable t m, NotReady t m, PostBuild t m
     )
  => Event t a
  -> Map
  -> GridConfig
  -> (Picture, Picture)
  -> Player t
  -> Width Float
  -> Height Float
  -> V2 Float
  -> m (Event t (Data.Map.Map Unique (Maybe (Box t))))
initBox eCreate mp gc pic player w h pos = do
  eBoxUnique <- requestUnique eCreate
  (_, eCreateBox) <-
    runWithReplace
      (pure ())
      (eBoxUnique <&>
       \u -> Data.Map.singleton u . Just <$> mkBox mp gc u pic w h pos player)
  pure eCreateBox

mkBox
  :: ( Reflex t, MonadHold t m, MonadFix m
     , UniqueSupply t m, RandomGen t m
     , NotReady t m, PostBuild t m, Adjustable t m
     )
  => Map
  -> GridConfig
  -> Unique
  -> (Picture, Picture)
  -> Width Float
  -> Height Float
  -> V2 Float
  -> Player t
  -> m (Box t)
mkBox mp gc u pic@(openPic, closedPic) _boxWidth _boxHeight bPos player = do
  _boxPosition <- mkEntityPosition mp _boxWidth _boxHeight bPos never never

  rec
    _boxOpen <- mkBoxOpen player box
    _boxOpenedFirstTime <- headE $ () <$ updated _boxOpen

    dBoxOpenedCount :: Dynamic t Int <- count . ffilter id $ updated _boxOpen
    let openedFiveTimes = () <$ ffilter (>=5) (updated dBoxOpenedCount)

    let _boxPicture = mkBoxPicture (openPic, closedPic) _boxOpen

    _boxUpdate <- mkBoxUpdate mp gc u player pic _boxOpenedFirstTime openedFiveTimes
    let _boxQuadrants = getQuadrants'' gc box

    let box = Box{..}

  pure box

initBoxes
  :: forall t m
   . (Reflex t, MonadHold t m, MonadFix m)
  => Event t (Data.Map.Map Unique (Maybe (Box t)))
  -> m (Event t (Data.Map.Map Unique (Maybe (Box t))))
initBoxes externalBoxUpdates = mdo
  let
    eBoxesUpdated :: Event t (Data.Map.Map Unique (Maybe (Box t)))
    eBoxesUpdated =
      externalBoxUpdates <>
      switchDyn deBoxUpdate

  deBoxUpdate :: Dynamic t (Event t (Data.Map.Map Unique (Maybe (Box t)))) <-
    accumDyn (<>) never (foldMapOf (folded.folded) (view boxUpdate) <$> eBoxesUpdated)

  pure eBoxesUpdated
