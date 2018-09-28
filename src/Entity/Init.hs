{-# language ScopedTypeVariables #-}
module Entity.Init where

import Reflex
import Reflex.Workflow (Workflow(..), workflow)
import Control.Monad.Fix (MonadFix)
import Linear.V2 (V2(..))

import RandomGen.Class (RandomGen, randomPosition)
import Unique (Unique)
import UniqueSupply.Class (UniqueSupply, withUnique)

mkPosNotOn
  :: ( Reflex t, MonadHold t m, MonadFix m
     , UniqueSupply t m, RandomGen t m
     , Adjustable t m
     )
  => (Int, Int)
  -> (Int, Int)
  -> Dynamic t (V2 Float)
  -> Event t a
  -> m (Event t (V2 Float))
mkPosNotOn xBounds yBounds dPos eCreate = switchDyn <$> workflow w
  where
    w = Workflow $ do
      eRandomPos :: Event t (V2 Float) <-
        fmap (uncurry V2) <$> randomPosition eCreate xBounds yBounds
      let eRetry = ffilter id ((==) <$> current dPos <@> eRandomPos)
      pure (eRandomPos, w <$ eRetry)

mkUniqueAndPosNotOn
  :: forall t m a
   . ( MonadHold t m, MonadFix m
     , UniqueSupply t m, RandomGen t m
     , Adjustable t m
     )
  => (Int, Int)
  -> (Int, Int)
  -> Dynamic t (V2 Float)
  -> Event t a
  -> m (Event t (Unique, V2 Float))
mkUniqueAndPosNotOn xBounds yBounds dPos eCreate = do
  eRandomPos :: Event t (V2 Float) <- mkPosNotOn xBounds yBounds dPos eCreate
  withUnique eCreate (\u -> (,) u <$> eRandomPos)

mkPosNotOnWith
  :: forall t m a b
   . ( MonadHold t m, MonadFix m
     , UniqueSupply t m, RandomGen t m
     , Adjustable t m
     )
  => (Int, Int)
  -> (Int, Int)
  -> Dynamic t (V2 Float)
  -> Event t a
  -> (a -> V2 Float -> b)
  -> m (Event t b)
mkPosNotOnWith xBounds yBounds dPos eValue f = do
  eRandomPos :: Event t (V2 Float) <- mkPosNotOn xBounds yBounds dPos eValue
  switchHoldPromptly never $ (\v -> f v <$> eRandomPos) <$> eValue
