{-# language ScopedTypeVariables #-}
module Entity.Init where

import Reflex
import Reflex.Workflow (Workflow(..), workflow)
import Control.Monad.Fix (MonadFix)
import Data.Map (Map)
import Linear.V2 (V2(..))

import qualified Data.Map as Map

import RandomGen.Class (RandomGen, randomPosition)
import Unique (Unique)
import UniqueSupply.Class (UniqueSupply, withUnique)

mkUniqueAndPos
  :: forall t m a
   . ( MonadHold t m
     , UniqueSupply t m, RandomGen t m
     )
  => Event t a
  -> m (Event t (Unique, V2 Float))
mkUniqueAndPos eCreate = do
  eRandomPos :: Event t (Float, Float) <-
    randomPosition eCreate (0, 990) (0, 990)
  withUnique
    eCreate
    (\u -> (\(x, y) -> (u, V2 x y)) <$> eRandomPos)

mkUniqueAndPosNotOnPosition
  :: ( Reflex t, MonadHold t m, MonadFix m
     , UniqueSupply t m, RandomGen t m
     , Adjustable t m
     )
  => Dynamic t (V2 Float)
  -> Event t a
  -> m (Event t (Unique, V2 Float))
mkUniqueAndPosNotOnPosition dPos eCreate = switchDyn <$> workflow w
  where
    w = Workflow $ do
      eRandomPos <- mkUniqueAndPos eCreate
      let
        eRetry =
          ffilter id
          ((\a -> (a ==) . snd) <$>
            current dPos <@>
            eRandomPos)
      pure (eRandomPos, w <$ eRetry)
