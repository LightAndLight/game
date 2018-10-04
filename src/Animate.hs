{-# language ScopedTypeVariables #-}
module Animate where

import Reflex
import Control.Monad.Fix (MonadFix)
import Graphics.Gloss (Picture, blank)

loopWithDelay
  :: forall  t m
   . (Reflex t, MonadHold t m, MonadFix m)
  => Int
  -> [Picture]
  -> Event t ()
  -> m (Dynamic t Picture)
loopWithDelay del ps e = do
  dCount :: Dynamic t Int <- fmap (`mod` del) <$> count e
  fmap head <$>
    accumDyn (\p _ -> tail p) pics (ffilter (== del-1) $ updated dCount)
  where
    pics =
      case ps of
        [] -> repeat blank
        _:_ -> cycle ps

loop
  :: (Reflex t, MonadHold t m, MonadFix m)
  => [Picture]
  -> Event t ()
  -> m (Dynamic t Picture)
loop ps e = fmap head <$> accumDyn (\p _ -> tail p) pics e
  where
    pics =
      case ps of
        [] -> repeat blank
        _:_ -> cycle ps
