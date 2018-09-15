{-# language RecordWildCards #-}
module Controls where

import Control.Monad.Fix (MonadFix)
import Graphics.Gloss.Interface.IO.Game hiding (Event, Point)
import Reflex
  (Dynamic, Event, MonadHold, Reflex, holdUniqDyn, holdDyn, fforMaybe)
import Reflex.Gloss (InputEvent)

charDown :: Reflex t => Char -> Event t InputEvent -> Event t Bool
charDown c eIn =
  fforMaybe eIn $
  \e -> case e of
    EventKey (Char c') dir _ _ | c == c' ->
      case dir of
        Down -> Just True
        Up -> Just False
    _ -> Nothing

specialKeyPressed :: Reflex t => SpecialKey -> Event t InputEvent -> Event t ()
specialKeyPressed sk eIn =
  fforMaybe eIn $
  \e -> case e of
    EventKey (SpecialKey sk') Down _ _ | sk == sk' -> Just ()
    _ -> Nothing

data Controls t
  = Controls
  { _dWHeld :: Dynamic t Bool
  , _dSHeld :: Dynamic t Bool
  , _dAHeld :: Dynamic t Bool
  , _dDHeld :: Dynamic t Bool
  , _eSpacePressed :: Event t ()
  , _eRefresh :: Event t Float
  }

mkControls
  :: (MonadHold t m, Reflex t, MonadFix m)
  => Event t Float
  -> Event t InputEvent
  -> m (Controls t)
mkControls _eRefresh eInput = do
  _dWHeld <- holdUniqDyn =<< holdDyn False (charDown 'w' eInput)
  _dSHeld <- holdUniqDyn =<< holdDyn False (charDown 's' eInput)
  _dAHeld <- holdUniqDyn =<< holdDyn False (charDown 'a' eInput)
  _dDHeld <- holdUniqDyn =<< holdDyn False (charDown 'd' eInput)
  let _eSpacePressed = specialKeyPressed KeySpace eInput
  pure $ Controls{..}
