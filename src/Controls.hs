{-# language RecordWildCards #-}
module Controls where

import Reflex
import Reflex.Gloss.Event
  (GlossEvent(..), Key(..), KeyState(..), SpecialKey(..))
import Control.Monad (void)
import Control.Monad.Fix (MonadFix)

charDown
  :: Reflex t
  => Char
  -> EventSelector t GlossEvent
  -> Event t Bool
charDown c input =
  leftmost
  [ True <$ select input (GE_Key (Just $ Char c) (Just Down) Nothing)
  , False <$ select input (GE_Key (Just $ Char c) (Just Up) Nothing)
  ]

specialKeyPressed
  :: Reflex t
  => SpecialKey
  -> EventSelector t GlossEvent
  -> Event t ()
specialKeyPressed sk input =
  void $ select input (GE_Key (Just $ SpecialKey sk) (Just Down) Nothing)

data Controls t
  = Controls
  { _dWHeld :: Dynamic t Bool
  , _dSHeld :: Dynamic t Bool
  , _dAHeld :: Dynamic t Bool
  , _dDHeld :: Dynamic t Bool
  , _eSpacePressed :: Event t ()
  , _eEscPressed :: Event t ()
  , _eRefresh :: Event t Float
  }

mkControls
  :: (MonadHold t m, Reflex t, MonadFix m)
  => Event t Float
  -> EventSelector t GlossEvent
  -> m (Controls t)
mkControls _eRefresh input = do
  _dWHeld <- holdUniqDyn =<< holdDyn False (charDown 'w' input)
  _dSHeld <- holdUniqDyn =<< holdDyn False (charDown 's' input)
  _dAHeld <- holdUniqDyn =<< holdDyn False (charDown 'a' input)
  _dDHeld <- holdUniqDyn =<< holdDyn False (charDown 'd' input)
  let
    _eSpacePressed = specialKeyPressed KeySpace input
    _eEscPressed = specialKeyPressed KeyEsc input
  pure $ Controls{..}
