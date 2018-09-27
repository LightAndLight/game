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
import Control.Lens.TH (makeLenses)
import Control.Monad.Fix (MonadFix)
import Linear.V2 (V2(..))

import qualified Data.Map

import Dimensions (Width(..), Height(..), HasWidth(..), HasHeight(..))
import Entity.Init (mkUniqueAndPosNotOnPosition)
import Entity.Intersects (intersects)
import Entity.Picture (HasPicture(..))
import Entity.Position (HasPosition(..), mkEntityPosition)
import Entity.Quadrants (HasQuadrants(..))
import Graphics.Gloss (Picture)
import Grid.Quadrant (Quadrant)
import Map (Map(..))
import Player (Player(..))
import RandomGen.Class (RandomGen)
import Unique (Unique)
import UniqueSupply.Class (UniqueSupply)

data Box t
  = Box
  { _boxQuadrants :: Dynamic t [Quadrant]
  , _boxOpen :: Dynamic t Bool
  , _boxOpenedFirstTime :: Event t ()
  , _boxPicture :: Dynamic t Picture
  , _boxPosition :: Dynamic t (V2 Float)
  , _boxWidth :: Width Float
  , _boxHeight :: Height Float
  , _boxUpdate :: Event t (Data.Map.Map Unique (Maybe (V2 Float)))
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
  => Player t
  -> Event t ()
  -> m (Event t (Data.Map.Map Unique (Maybe (V2 Float))))
mkBoxUpdate Player{..} openedFirstTime = do
  eeUpdate <-
    networkView =<<
    holdDyn
      (pure never)
      (mkUniqueAndPosNotOnPosition _playerPosition openedFirstTime <$
       openedFirstTime)

  switchHold never eeUpdate

mkBox
  :: ( Reflex t, MonadHold t m, MonadFix m
     , UniqueSupply t m, RandomGen t m
     , NotReady t m, PostBuild t m, Adjustable t m
     )
  => Map
  -> Dynamic t [Quadrant]
  -> (Picture, Picture)
  -> Width Float
  -> Height Float
  -> V2 Float
  -> Player t
  -> m (Box t)
mkBox mp _boxQuadrants (openPic, closedPic) _boxWidth _boxHeight bPos player = do
  _boxPosition <- mkEntityPosition mp _boxWidth _boxHeight bPos never never

  rec
    _boxOpen <- mkBoxOpen player box
    _boxOpenedFirstTime <- headE $ () <$ updated _boxOpen

    let _boxPicture = mkBoxPicture (openPic, closedPic) _boxOpen

    _boxUpdate <- mkBoxUpdate player _boxOpenedFirstTime

    let box = Box{..}

  pure box
