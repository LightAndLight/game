{-# language LambdaCase #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables #-}
module AABBTree where

import Reflex
import Control.Monad (join)
import Control.Monad.Fix (MonadFix)
import Numeric.Interval (Interval, hull)
import qualified Numeric.Interval as Interval

data BB n = BB { bbX :: !(Interval n), bbY :: !(Interval n) }
  deriving (Eq, Show)

bbHull :: Ord n => BB n -> BB n -> BB n
bbHull (BB a b) (BB a' b') = BB (hull a a') (hull b b')

bbIntersects :: Ord n => BB n -> BB n -> Bool
bbIntersects (BB a b) (BB a' b') =
  not $
    Interval.null (Interval.intersection a a') ||
    Interval.null (Interval.intersection b b')

bbContains :: Ord n => BB n -> BB n -> Bool
bbContains (BB a b) (BB a' b') =
  Interval.contains a a' && Interval.contains b b'

bbArea :: Num n => BB n -> n
bbArea (BB a b) = Interval.width a * Interval.width b

data Tree' n a
  = Leaf (BB n) a
  | Branch !Int (BB n) (Tree' n a) (Tree' n a)
  deriving (Eq, Show)

data Tree n a
  = Empty
  | Tree (Tree' n a)
  deriving (Eq, Show)

empty :: Tree n a
empty = Empty

treeBB :: Tree n a -> BB n
treeBB Empty = BB Interval.empty Interval.empty
treeBB (Tree t) = treeBB' t

treeBB' :: Tree' n a -> BB n
treeBB' (Leaf bb _) = bb
treeBB' (Branch _ bb _ _) = bb

treeSize :: Tree n a -> Int
treeSize Empty = 0
treeSize (Tree t) = treeSize' t

treeSize' :: Tree' n a -> Int
treeSize' Leaf{} = 1
treeSize' (Branch n _ _ _) = n


-- | time complexity
--
-- I think this insert is O(log n) because of the balanced-ness
--
-- how do I prove this?
insert :: (Num n, Ord n) => (a, BB n) -> Tree n a -> Tree n a
insert (a, bb) Empty = Tree $ Leaf bb a
insert item (Tree tree) =
  Tree $ go item tree (bbHull (snd item) $ treeBB' tree)
  where
    go (a, bb) l@Leaf{} newHull = Branch 2 newHull (Leaf bb a) l
    go a (Branch sz _ l r) newHull =
      let
        hullL = bbHull (snd a) $ treeBB' l
        hullR = bbHull (snd a) $ treeBB' r
      in
        case compare (bbArea hullL) (bbArea hullR) of
          LT -> Branch (sz+1) newHull (go a l hullL) r
          EQ ->
            if treeSize' l <= treeSize' r
            then Branch (sz+1) newHull (go a l hullL) r
            else Branch (sz+1) newHull l (go a r hullR)
          GT -> Branch (sz+1) newHull l (go a r hullR)

-- | extract a list of things which are colliding with a bounding box
collidingWith :: Ord n => BB n -> Tree n a -> [a]
collidingWith _ Empty = []
collidingWith bb (Tree tree) = go tree []
  where
    go (Leaf bb' a) =
      if bb `bbIntersects` bb'
      then (a :)
      else id
    go (Branch _ bb' l r) =
      if bb `bbIntersects` bb'
      then go l . go r
      else id

-- | extract a list of things which are colliding
collisions :: Ord n => Tree n a -> [(a, a)]
collisions Empty = []
collisions (Tree Leaf{}) = []
collisions (Tree (Branch _ _ left right)) = go left right []
  where
    go (Leaf bb a) (Leaf bb' a') =
      if bb `bbIntersects` bb'
      then ((a, a') :)
      else id
    go leaf@(Leaf bb _) (Branch _ bb' l r) =
      go l r .
      if bb `bbIntersects` bb'
      then go leaf l . go leaf r
      else id
    go (Branch _ bb l r) leaf@(Leaf bb' _) =
      go l r .
      if bb `bbIntersects` bb'
      then go l leaf . go r leaf
      else id
    go (Branch _ bb l r) (Branch _ bb' l' r') =
      go l r . go l' r' .
      if bb `bbIntersects` bb'
      then
        go l l' . go l r' .
        go r l' . go r r'
      else id

-- | delete all elements which satisfy a predicate
--
-- O(n)
--
-- insert . delete is fine for infrequent updates, but for frequent
-- large-scale updates it's faster to rebuild the entire tree using insert.
delete :: Ord n => (a -> Bool) -> Tree n a -> Tree n a
delete _ Empty = Empty
delete p (Tree tree) = go tree
  where
    go l@(Leaf _ a) =
      if p a
      then Empty
      else Tree l
    go (Branch _ _ l r) =
      let
        l' = go l
        r' = go r
      in
        case l' of
          Empty -> r'
          Tree l'' ->
            case r' of
              Empty -> Tree l''
              Tree r'' ->
                Tree $
                Branch
                  (treeSize' l'' + treeSize' r'')
                  (bbHull (treeBB' l'') (treeBB' r''))
                  l''
                  r''

data TT a
  = TwoE a
  | ThreeE a a
  | TwoF !Int a (TT a) (TT a)
  | ThreeF !Int a a (TT a) (TT a) (TT a)
  deriving Show

ttSize :: TT a -> Int
ttSize TwoE{} = 1
ttSize ThreeE{} = 2
ttSize (TwoF s _ _ _) = s
ttSize (ThreeF s _ _ _ _ _) = s

ttInsert :: Ord a => a -> TT a -> TT a
ttInsert a t =
  case go t of
    Left t' -> t'
    Right (m, l, r) -> TwoF (1 + ttSize l + ttSize r) m l r
  where
    go tr@(TwoE b) =
      Left $ case compare a b of
        LT -> ThreeE a b
        EQ -> tr
        GT -> ThreeE b a
    go tr@(ThreeE b c) =
      case compare a b of
        LT -> Right (b, TwoE a, TwoE c) -- b is the middle
        EQ -> Left tr
        GT ->
          case compare a c of
            LT -> Right (a, TwoE b, TwoE c) -- a is the middle
            EQ -> Left tr
            GT -> Right (c, TwoE b, TwoE a) -- c is the middle
    go (TwoF sz b l r) =
      if a < b
      then case go l of
        Left l' -> Left $ TwoF (sz+1) b l' r
        Right (c, l', m) ->
          Left $ ThreeF (2 + ttSize l' + ttSize m + ttSize r) c b l' m r
      else case go r of
        Left r' -> Left $ TwoF (sz+1) b l r'
        Right (c, m, r') ->
          Left $ ThreeF (2 + ttSize l + ttSize m + ttSize r') b c l m r'
    go (ThreeF sz b c l m r) =
      if a < b
      then case go l of
        Left l' -> Left $ ThreeF (sz+1) b c l' m r
        Right (d, l1, l2) ->
          -- l1 l2 m r
          Right
            ( b
            , TwoF (1 + ttSize l1 + ttSize l2) d l1 l2
            , TwoF (1 + ttSize m + ttSize r) c m r
            )
      else
        if a < c
        then case go m of
          Left m' -> Left $ ThreeF (sz+1) b c l m' r
          Right (d, m1, m2) ->
            -- l m1 m2 r
            Right
              ( d
              , TwoF (1 + ttSize l + ttSize m1) b l m1
              , TwoF (1 + ttSize m2 + ttSize r) c m2 r
              )
        else case go r of
          Left r' -> Left $ ThreeF (sz+1) b c l m r'
          Right (d, r1, r2) ->
            -- l m r1 r2
            Right
              ( c
              , TwoF (1 + ttSize l + ttSize m) b l m
              , TwoF (1 + ttSize r1 + ttSize r2) d r1 r2
              )

newtype List t a = List { unList :: Dynamic t (List' t a) }
data List' t a = Nil | Cons a (List t a)

listHold
  :: forall t m a r
   . (Reflex t, MonadHold t m, MonadFix m)
  => Event t a
  -> Event t (r, a -> List t a -> r)
  -> m (Event t r, List t a)
listHold eCons eUncons = do
  rec
    d' :: Dynamic t (List' t a) <- holdDyn Nil (flip Cons (List d) <$> eCons)
    d :: Dynamic t (List' t a) <-
      join <$>
      holdDyn
        d'
        ((d' >>= (\case; Nil -> d'; Cons _ d'' -> unList d'')) <$
         eUncons)

  pure
    ( (\l (n, c) ->
         case l of
           Nil -> n
           Cons x y -> c x y) <$>
      current d' <@>
      eUncons
    , List d
    )
  {-
  rec
    dConsed <-
      holdDyn Nil $
      flip Cons dConsed <$> eCons
  let
    eUnconsed =
      (\case; Nil -> Nothing; Cons a b -> Just (a, b)) <$>
      current dConsed <@
      eUncons

  pure (eUnconsed, dConsed)
-}

{-
data Action = Insert | Remove

data TT' t a
  = TwoE' a
  | ThreeE' a a
  | TwoF'
      (Dynamic t Int)
      a
      (Dynamic t (TT' t a))
      (Dynamic t (TT' t a))
  | ThreeF'
      (Dynamic t Int)
      a a
      (Dynamic t (TT' t a))
      (Dynamic t (TT' t a))
      (Dynamic t (TT' t a))

ttSize' :: Reflex t => TT' t a -> Dynamic t Int
ttSize' TwoE'{} = pure 1
ttSize' ThreeE'{} = pure 2
ttSize' (TwoF' s _ _ _) = s
ttSize' (ThreeF' s _ _ _ _ _) = s

ttHold
  :: forall t m a
   . (Reflex t, MonadHold t m, MonadFix m, Ord a)
  => a
  -> Event t (Action, a)
  -> m (Dynamic t (TT' t a))
ttHold initial eUpdate = mdo
  dCarry
    :: Dynamic t
         (Either
           (Dynamic t (TT' t a))
           (a, Dynamic t (TT' t a), Dynamic t (TT' t a))) <-
    holdDyn
      (Left . pure $ TwoE' initial)
      (run <$>
       current dActual <@>
       eUpdate)
  let
    dActual :: Dynamic t (TT' t a)
    dActual =
      (either
        id
        (\(m, l, r) ->
           pure $
           TwoF'
             ((+) . (1+) <$> (l >>= ttSize') <*> (r >>= ttSize'))
             m
             l
             r)) =<<
      dCarry
  pure dActual
  where
    run
      :: TT' t a
      -> (Action, a)
      -> Either
           (Dynamic t (TT' t a))
           (a, Dynamic t (TT' t a), Dynamic t (TT' t a))
    run t (action, x) =
      case t of
        TwoE' a ->
          case action of
            Insert ->
              case compare a x of
                LT -> Left . pure $ ThreeE' a x
                EQ -> Left $ pure t
                GT -> Left . pure $ ThreeE' x a
            Remove ->
              if a == x
              then error "removing only element"
              else Left $ pure t
        ThreeE' a b ->
          case action of
            Insert ->
              case compare x a of
                LT -> Right (a, pure $ TwoE' x, pure $ TwoE' b)
                EQ -> Left $ pure t
                GT ->
                  case compare x b of
                    LT -> Right (x, pure $ TwoE' a, pure $ TwoE' b)
                    EQ -> Left $ pure t
                    GT -> Right (b, pure $ TwoE' a, pure $ TwoE' x)
            Remove ->
              if a == x
              then Left . pure $ TwoE' b
              else if b == x
                then Left . pure $ TwoE' a
                else Left $ pure t
        TwoF' dSz a dL dR ->
          case action of
            Insert ->
              if x < a
              then
                Left $
                (\tt ->
                   case run tt (action, a) of
                     Left dL' -> TwoF' ((1+) <$> dSz) a dL' dR
                     Right (b, dL', dM) ->
                       ThreeF'
                         ((\ls ms rs -> 2 + ls + ms + rs) <$>
                          (dL' >>= ttSize') <*>
                          (dM >>= ttSize') <*>
                          (dR >>= ttSize'))
                         b a
                         dL' dM dR) <$>
                dL
              else
                Left $
                (\tt ->
                   case run tt (action, a) of
                     Left dR' -> TwoF' ((1+) <$> dSz) a dL dR'
                     Right (b, dM, dR') ->
                       ThreeF'
                         ((\ls ms rs -> 2 + ls + ms + rs) <$>
                          (dL >>= ttSize') <*>
                          (dM >>= ttSize') <*>
                          (dR' >>= ttSize'))
                         b a
                         dL dM dR') <$>
                dR
            Remove -> undefined
        ThreeF' dSz a b dL dM dR ->
          if x < a
          then _
            {-
            case go l of
            Left l' -> Left . pure $ ThreeF ((+1) <$> dSz) a b l' m r
            Right (d, l1, l2) ->
              -- l1 l2 m r
              Right
                ( a
                , TwoF (1 + ttSize l1 + ttSize l2) d l1 l2
                , TwoF (1 + ttSize m + ttSize r) b m r
                )
-}
          else
            if x < b
            then _
{-
              case go m of
              Left m' -> Left $ ThreeF (sz+1) a b l m' r
              Right (d, m1, m2) ->
                -- l m1 m2 r
                Right
                  ( d
                  , TwoF (1 + ttSize l + ttSize m1) a l m1
                  , TwoF (1 + ttSize m2 + ttSize r) b m2 r
                  )
-}
            else _
              {- case go r of
              Left r' -> Left $ ThreeF (sz+1) a b l m r'
              Right (d, r1, r2) ->
                -- l m r1 r2
                Right
                  ( b
                  , TwoF (1 + ttSize l + ttSize m) a l m
                  , TwoF (1 + ttSize r1 + ttSize r2) d r1 r2
                  )
-}

-}
