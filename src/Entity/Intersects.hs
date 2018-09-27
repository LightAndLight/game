module Entity.Intersects where

import Reflex
import Control.Lens.Getter ((^.))
import Linear.V2 (_x, _y)

import Dimensions (Width(..), Height(..), HasWidth(..), HasHeight(..))
import Entity.Position (HasPosition(..))
import Entity.Quadrants (HasQuadrants(..))

intersects
  :: ( Reflex t
     , HasPosition t a, HasWidth a, HasHeight a, HasQuadrants t a
     , HasPosition t b, HasWidth b, HasHeight b, HasQuadrants t b
     )
  => a
  -> b
  -> Dynamic t Bool
intersects e1 e2 =
  (\e1Qs e2Qs e1Left e1Top e2Left e2Top ->
    let
      e1Right = e1Left + unWidth (e1^.width)
      e1Bottom = e1Top + unHeight (e1^.height)
      e2Right = e2Left + unWidth (e2^.width)
      e2Bottom = e2Top + unHeight (e2^.height)
    in
      any (`elem` e1Qs) e2Qs &&
      not
        (e1Right < e2Left ||
         e1Top > e2Bottom ||
         e1Left > e2Right ||
         e1Bottom < e2Top)) <$>
  (e1^.quadrants) <*>
  (e2^.quadrants) <*>
  ((^. _x) <$> e1^.position) <*>
  ((^. _y) <$> e1^.position) <*>
  ((^. _x) <$> e2^.position) <*>
  ((^. _y) <$> e2^.position)
