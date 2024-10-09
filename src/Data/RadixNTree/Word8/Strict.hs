{-# LANGUAGE BangPatterns
           , GADTs
           , RankNTypes
           , ScopedTypeVariables
           , UnboxedTuples #-}

module Data.RadixNTree.Word8.Strict
  ( StrictRadixTree
  , RadixTree (..)

  , StrictRadix1Tree
  , Radix1Tree (..)

  , empty0
  , empty1

  , singleton0
  , singleton1

  , map0
  , map0'
  , mapWithKey0
  , mapWithKey0'

  , map1
  , map1'
  , mapWithKey1
  , mapWithKey1'

  , foldl0
  , foldl0'
  , foldlWithKey0
  , foldlWithKey0'

  , Data.RadixNTree.Word8.Strict.foldl1
  , foldl1'
  , foldlWithKey1
  , foldlWithKey1'

  , foldr0
  , foldr0'
  , foldrWithKey0
  , foldrWithKey0'

  , Data.RadixNTree.Word8.Strict.foldr1
  , foldr1'
  , foldrWithKey1
  , foldrWithKey1'

  , foldMap0
  , foldMapWithKey0

  , foldMap1
  , foldMapWithKey1

  , traverse0
  , traverseWithKey0

  , traverse1
  , traverseWithKey1

  , null0
  , null1

  , size0
  , size1

  , lookup0
  , find0
  , member0
  , subtree0
  , prefix0

  , lookup1
  , find1
  , member1
  , subtree1
  , prefix1

  , lookupL0
  , lookupL1

  , lookupR0
  , lookupR1

  , adjustL0
  , adjustL0'
  , adjustLWithKey0
  , adjustLWithKey0'

  , adjustL1
  , adjustL1'
  , adjustLWithKey1
  , adjustLWithKey1'

  , adjustR0
  , adjustR0'
    , adjustRWithKey0
  , adjustRWithKey0'

  , adjustR1
  , adjustR1'
  , adjustRWithKey1
  , adjustRWithKey1'

  , updateL0
  , updateLWithKey0

  , updateL1
  , updateLWithKey1

  , updateR0
  , updateRWithKey0

  , updateR1
  , updateRWithKey1

  , takeL0
  , takeL1

  , takeR0
  , takeR1

  , union0
  , union1

  , unionL0
  , unionL1

  , unionWith0'
  , unionWith1'

  , unionWithKey0'
  , unionWithKey1'

  , difference0
  , difference1

  , differenceWith0
  , differenceWith1

  , differenceWithKey0
  , differenceWithKey1

  , compare0
  , Data.RadixNTree.Word8.Strict.compare1

  , disjoint0
  , disjoint1

  , intersection0
  , intersection1

  , intersectionL0
  , intersectionL1

  , intersectionWith0'
  , intersectionWith1'

  , intersectionWithKey0'
  , intersectionWithKey1'

  , merge0
  , merge1

  , insert0
  , insert1

  , insertWith0
  , insertWith0'

  , insertWith1
  , insertWith1'

  , adjust0
  , adjust0'

  , adjust1
  , adjust1'

  , delete0
  , delete1

  , prune0
  , prune1

  , update0
  , update1

  , alter0
  , alter1

  , shape0
  , shape1

  , Split (..)
  , Split1 (..)
  , splitL0
  , splitL1

  , SplitLookup (..)
  , SplitLookup1 (..)
  , splitLookup0
  , splitLookup1

  , filter0
  , filterWithKey0

  , filter1
  , filterWithKey1

  , mapMaybe0
  , mapMaybeWithKey0

  , mapMaybe1
  , mapMaybeWithKey1

  , partition0
  , partitionWithKey0

  , partition1
  , partitionWithKey1

  , mapEither0
  , mapEitherWithKey0

  , mapEither1
  , mapEitherWithKey1

  , lookupMin0
  , lookupMin1
  , unsafeLookupMin1

  , lookupMinWithKey0
  , lookupMinWithKey1
  , unsafeLookupMinWithKey1

  , lookupMax0
  , lookupMax1
  , unsafeLookupMax1

  , lookupMaxWithKey0
  , lookupMaxWithKey1
  , unsafeLookupMaxWithKey1

  , deleteMin0
  , deleteMin1
  , unsafeDeleteMin1

  , deleteMax0
  , deleteMax1
  , unsafeDeleteMax1

  , adjustMin0
  , adjustMin1
  , unsafeAdjustMin1

  , adjustMin0'
  , adjustMin1'
  , unsafeAdjustMin1'

  , adjustMinWithKey0
  , adjustMinWithKey1
  , unsafeAdjustMinWithKey1

  , adjustMinWithKey0'
  , adjustMinWithKey1'
  , unsafeAdjustMinWithKey1'

  , adjustMax0
  , adjustMax1
  , unsafeAdjustMax1

  , adjustMax0'
  , adjustMax1'
  , unsafeAdjustMax1'

  , adjustMaxWithKey0
  , adjustMaxWithKey1
  , unsafeAdjustMaxWithKey1

  , adjustMaxWithKey0'
  , adjustMaxWithKey1'
  , unsafeAdjustMaxWithKey1'

  , updateMin0
  , updateMin1
  , unsafeUpdateMin1

  , updateMinWithKey0
  , updateMinWithKey1
  , unsafeUpdateMinWithKey1

  , updateMax0
  , updateMax1
  , unsafeUpdateMax1

  , updateMaxWithKey0
  , updateMaxWithKey1
  , unsafeUpdateMaxWithKey1

  , ViewL (..)
  , ViewL1 (..)
  , minView0
  , minView1
  , unsafeMinView1

  , ViewR (..)
  , ViewR1 (..)
  , maxView0
  , maxView1
  , unsafeMaxView1
  ) where

import           Data.ByteArray.NonEmpty
import           Data.RadixNTree.Word8.Common
import           Data.RadixNTree.Word8.Key
import           Radix.Common
import           Radix.Exception
import           Radix.Word8.Foundation

import           Control.Applicative
import           Control.Exception (throw)
import           Control.DeepSeq
import           Data.Bits
import           Data.Foldable
import           Data.Functor.Classes
import           Data.Primitive.ByteArray
import           Data.Word
import           Text.Show



-- | Convenience type synonym.
type StrictRadixTree = RadixTree

-- | Spine-strict radix tree with byte sequences as keys.
data RadixTree a = RadixTree
                     {-# UNPACK #-} !(Maybe a) -- ^ Value at the empty byte sequence key.
                     !(Radix1Tree a)

instance Show a => Show (RadixTree a) where
  showsPrec = liftShowsPrec showsPrec showList

instance Show1 RadixTree where
  liftShowsPrec showsPrec_ showList_ d t =
    showParen (d > 10) $
      showListWith (liftShowsPrec showsPrec_ showList_ 0) $
        foldrWithKey0 (\k a -> (:) (k, a)) [] t

instance Eq a => Eq (RadixTree a) where
  (==) = liftEq (==)

instance Eq1 RadixTree where
  liftEq eq (RadixTree mx l) (RadixTree my r) = liftEq eq mx my && liftEq eq l r

-- | Uses 'Data.RadixTree.Word8.Strict.map'.
instance Functor RadixTree where
  fmap = map0

instance Foldable RadixTree where
  foldl = foldl0
  foldr = foldr0
  foldMap = foldMap0

  foldl' = foldl0'
  foldr' = foldr0'

  null = null0

  length = size0

instance Traversable RadixTree where
  traverse = traverse0


instance NFData a => NFData (RadixTree a) where
  rnf = liftRnf rnf

instance NFData1 RadixTree where
  liftRnf nf (RadixTree mx t) = liftRnf nf mx `seq` liftRnf nf t



-- | Convenience type synonym.
type StrictRadix1Tree = Radix1Tree

-- | Spine-strict radix tree with non-empty byte sequences as keys.
data Radix1Tree a = Bin
                      {-# UNPACK #-} !Prefix
                      !(Radix1Tree a)        -- ^ Masked bit is @0@. Invariant: not 'Nil'.
                      !(Radix1Tree a)        -- ^ Masked bit is @1@. Invariant: not 'Nil'.

                  | Tip
                      {-# UNPACK #-} !ByteArray -- ^ Invariant: non-empty.
                      {-# UNPACK #-} !(Maybe a) -- ^ Invariant: can only be 'Nothing' when
                                                --   the tree below is 'Bin'.
                      !(Radix1Tree a)

                  | Nil

instance Show a => Show (Radix1Tree a) where
  showsPrec = liftShowsPrec showsPrec showList

instance Show1 Radix1Tree where
  liftShowsPrec showsPrec_ showList_ d t =
    showParen (d > 10) $
      showListWith (liftShowsPrec showsPrec_ showList_ 0) $
        foldrWithKey1 (\k a -> (:) (k, a)) [] t

instance Eq a => Eq (Radix1Tree a) where
  (==) = liftEq (==)

instance Eq1 Radix1Tree where
  liftEq eq = go
    where
      go l r =
        case l of
          Bin p xl xr ->
            case r of
              Bin q yl yr -> p == q && go xl yl && go xr yr
              _           -> False

          Tip arr mx dx ->
            case r of
              Tip brr my dy -> arr == brr && liftEq eq mx my && go dx dy
              _             -> False

          Nil ->
            case r of
              Nil -> True
              _   -> False

-- | Uses 'Data.Radix1Tree.Word8.Strict.map'.
instance Functor Radix1Tree where
  fmap = map1

instance Foldable Radix1Tree where
  foldl = Data.RadixNTree.Word8.Strict.foldl1
  foldr = Data.RadixNTree.Word8.Strict.foldr1
  foldMap = foldMap1

  foldl' = foldl1'
  foldr' = foldr1'

  null = null1

  length = size1

instance Traversable Radix1Tree where
  traverse = traverse1


instance NFData a => NFData (Radix1Tree a) where
  rnf = liftRnf rnf

instance NFData1 Radix1Tree where
  liftRnf nf = go
    where
      go t =
        case t of
          Bin _ l r   -> go l `seq` go r
          Tip _ mx dx -> liftRnf nf mx `seq` go dx
          Nil         -> ()



{-# INLINE join #-}
-- | Knowing that the prefices of two trees disagree, construct a 'Bin'.
join :: Prefix -> Radix1Tree a -> Prefix -> Radix1Tree a -> Radix1Tree a
join p0 t0 p1 t1 =
  let m = branchingBit p0 p1

      p = mask p0 m .|. m

  in if zeroBit p0 m
       then Bin p t0 t1
       else Bin p t1 t0

{-# INLINE safeJoin #-}
safeJoin :: Prefix -> Radix1Tree a -> Prefix -> Radix1Tree a -> Radix1Tree a
safeJoin _ Nil _  t1    = t1
safeJoin _ t0    _  Nil = t0
safeJoin p0 t0   p1 t1  = join p0 t0 p1 t1

{-# INLINE retip #-}
-- | Based on the altered entry and/or downward state, fuse or remove the 'Tip' as needed.
retip :: ByteArray -> Maybe a -> Radix1Tree a -> Radix1Tree a
retip arr mx dx =
  case mx of
    Just _  -> Tip arr mx dx
    Nothing ->
     case dx of
       Bin _ _ _     -> Tip arr mx dx
       Tip brr my dy -> Tip (appendByteArray arr brr) my dy
       Nil           -> Nil

{-# INLINE dropTrim #-}
dropTrim :: Int -> ByteArray -> Maybe a -> Radix1Tree a -> Radix1Tree a
dropTrim n arr mx dx =
  case mx of
    Just _  -> Tip (dropByteArray n arr) mx dx
    Nothing ->
     case dx of
       Bin _ _ _     -> Tip (dropByteArray n arr) mx dx
       Tip brr my dy -> Tip (dropAppendByteArray n arr brr) my dy
       Nil           -> Nil


{-# INLINE rebin #-}
rebin :: Prefix -> Radix1Tree a -> Radix1Tree a -> Radix1Tree a
rebin p l r =
  case l of
    Nil -> r
    _     -> case r of
               Nil -> l
               _     -> Bin p l r

{-# INLINE rebinL #-}
rebinL :: Prefix -> Radix1Tree a -> Radix1Tree a -> Radix1Tree a
rebinL p l r =
  case l of
    Nil -> r
    _   -> Bin p l r

{-# INLINE rebinR #-}
rebinR :: Prefix -> Radix1Tree a -> Radix1Tree a -> Radix1Tree a
rebinR p l r =
  case r of
    Nil -> l
    _   -> Bin p l r



empty0 :: RadixTree a
empty0 = RadixTree Nothing Nil

empty1 :: Radix1Tree a
empty1 = Nil



{-# INLINE singleton0 #-}
singleton0 :: Feed -> a -> RadixTree a
singleton0 (Feed feed) = \a ->
  feed $ \step s ->
    case step s of
      More w z -> RadixTree Nothing $ singleton1 (Feed1 w (\g -> g step z)) a
      Done     -> RadixTree (Just a) Nil

{-# INLINE singleton1 #-}
singleton1 :: Feed1 -> a -> Radix1Tree a
singleton1 (Feed1 w feed) = \a -> feed $ \step s -> singleton_ step w s a

{-# INLINE singleton_ #-}
-- | \(\mathcal{O}(1)\). Single element radix tree.
singleton_ :: (b -> Step Word8 b) -> Word8 -> b -> a -> Radix1Tree a
singleton_ step w s = \a -> Tip (fromStep step w s) (Just a) Nil



null0 :: RadixTree a -> Bool
null0 (RadixTree Nothing t) = null1 t
null0 _                     = False

null1 :: Radix1Tree a -> Bool
null1 Nil = True
null1 _   = False



size0 :: RadixTree a -> Int
size0 (RadixTree mx t) =
  let !n = size1 t
  in case mx of
       Just _  -> n + 1
       Nothing -> n

size1 :: Radix1Tree a -> Int
size1 = go 0
  where
    go z t =
      case t of
        Bin _ l r   -> let !n = go z l
                       in go n r

        Tip _ mx dx -> case mx of
                         Nothing -> go z dx
                         Just _  -> let !n = go z dx
                                    in n + 1
        Nil         -> z



{-# INLINE fmap' #-}
fmap' :: (a -> b) -> Maybe a -> Maybe b
fmap' f (Just x) = Just $! f x
fmap' _ Nothing  = Nothing



map0 :: (a -> b) -> RadixTree a -> RadixTree b
map0 f (RadixTree mx t) = RadixTree (fmap f mx) $ map1 f t

map1 :: (a -> b) -> Radix1Tree a -> Radix1Tree b
map1 f = go
  where
    go t =
      case t of
        Bin p l r     -> Bin p (go l) (go r)
        Tip arr mx dx -> Tip arr (fmap f mx) (go dx)
        Nil           -> Nil



map0' :: (a -> b) -> RadixTree a -> RadixTree b
map0' f (RadixTree mx t) = RadixTree (fmap' f mx) $ map1 f t

map1' :: (a -> b) -> Radix1Tree a -> Radix1Tree b
map1' f = go
  where
    go t =
      case t of
        Bin p l r     -> Bin p (go l) (go r)
        Tip arr mx dx -> Tip arr (fmap' f mx) (go dx)
        Nil           -> Nil



mapWithKey0 :: (Build -> a -> b) -> RadixTree a -> RadixTree b
mapWithKey0 f (RadixTree mx t) =
  RadixTree (f (Build Lin) <$> mx) $
    mapWithKey_ (\b arr -> f (Build $ Snoc b arr)) Lin t

mapWithKey1 :: (Build1 -> a -> b) -> Radix1Tree a -> Radix1Tree b
mapWithKey1 f = mapWithKey_ (\b arr -> f (Build1 $ b :/ arr)) Lin

{-# INLINE mapWithKey_ #-}
mapWithKey_
  :: (Tsil ByteArray -> ByteArray -> a -> b) -> Tsil ByteArray
  -> Radix1Tree a -> Radix1Tree b
mapWithKey_ f = go
  where
    go b t =
      case t of
        Bin p l r     -> Bin p (go b l) (go b r)
        Tip arr mx dx -> Tip arr (f b arr <$> mx) (go (Snoc b arr) dx)
        Nil           -> Nil



mapWithKey0' :: (Build -> a -> b) -> RadixTree a -> RadixTree b
mapWithKey0' f (RadixTree mx t) =
  RadixTree (f (Build Lin) `fmap'` mx) $
    mapWithKey'_ (\b arr -> f (Build $ Snoc b arr)) Lin t

mapWithKey1' :: (Build1 -> a -> b) -> Radix1Tree a -> Radix1Tree b
mapWithKey1' f = mapWithKey'_ (\b arr -> f (Build1 $ b :/ arr)) Lin

{-# INLINE mapWithKey'_ #-}
mapWithKey'_
  :: (Tsil ByteArray -> ByteArray -> a -> b) -> Tsil ByteArray
  -> Radix1Tree a -> Radix1Tree b
mapWithKey'_ f = go
  where
    go b t =
      case t of
        Bin p l r     -> Bin p (go b l) (go b r)
        Tip arr mx dx -> Tip arr (f b arr `fmap'` mx) (go (Snoc b arr) dx)
        Nil           -> Nil



foldl0 :: (b -> a -> b) -> b -> RadixTree a -> b
foldl0 f z (RadixTree mx t) =
  let z' = case mx of
             Just x  -> f z x
             Nothing -> z

  in Data.RadixNTree.Word8.Strict.foldl1 f z' t

foldl1 :: (b -> a -> b) -> b -> Radix1Tree a -> b
foldl1 f = go
  where
    go z t =
      case t of
        Bin _ l r   -> go (go z l) r

        Tip _ mx dx -> let z' = case mx of
                                  Just x  -> f z x
                                  Nothing -> z

                       in go z' dx

        Nil         -> z



foldl0' :: (b -> a -> b) -> b -> RadixTree a -> b
foldl0' f z (RadixTree mx t) =
  let !z' = case mx of
              Just x  -> f z x
              Nothing -> z

  in Data.RadixNTree.Word8.Strict.foldl1' f z' t

foldl1' :: (b -> a -> b) -> b -> Radix1Tree a -> b
foldl1' f = go
  where
    go !z t =
      case t of
        Bin _ l r   -> let !z' = go z l
                       in go z' r

        Tip _ mx dx -> let !z' = case mx of
                                   Just x  -> f z x
                                   Nothing -> z

                       in go z' dx

        Nil         -> z



foldlWithKey0 :: (b -> Build -> a -> b) -> b -> RadixTree a -> b
foldlWithKey0 f z (RadixTree mx t) =
  let z' = case mx of
             Just x  -> f z (Build Lin) x
             Nothing -> z

  in foldlWithKey_ (\z'' b arr -> f z'' (Build $ Snoc b arr)) z' t

foldlWithKey1 :: (b -> Build1 -> a -> b) -> b -> Radix1Tree a -> b
foldlWithKey1 f = foldlWithKey_ (\z b arr -> f z (Build1 $ b :/ arr))

{-# INLINE foldlWithKey_ #-}
foldlWithKey_ :: (b -> Tsil ByteArray -> ByteArray -> a -> b) -> b -> Radix1Tree a -> b
foldlWithKey_ f = go Lin
  where
    go b z t =
      case t of
        Bin _ l r     -> go b (go b z l) r

        Tip arr mx dx ->
          case mx of
            Nothing -> go (Snoc b arr) z dx
            Just a  -> go (Snoc b arr) (f z b arr a) dx

        Nil           -> z



foldlWithKey0' :: (b -> Build -> a -> b) -> b -> RadixTree a -> b
foldlWithKey0' f z (RadixTree mx t) =
  let !z' = case mx of
              Just x  -> f z (Build Lin) x
              Nothing -> z

  in foldlWithKey'_ (\z'' b arr -> f z'' (Build $ Snoc b arr)) z' t

foldlWithKey1' :: (b -> Build1 -> a -> b) -> b -> Radix1Tree a -> b
foldlWithKey1' f = foldlWithKey'_ (\z b arr -> f z (Build1 $ b :/ arr))

{-# INLINE foldlWithKey'_ #-}
foldlWithKey'_ :: (b -> Tsil ByteArray -> ByteArray -> a -> b) -> b -> Radix1Tree a -> b
foldlWithKey'_ f = go Lin
  where
    go b !z t =
      case t of
        Bin _ l r     -> let !z' = go b z l
                         in go b z' r

        Tip arr mx dx ->
          case mx of
            Nothing -> go (Snoc b arr) z dx
            Just a  -> let !z' = f z b arr a
                       in go (Snoc b arr) z' dx

        Nil           -> z



foldr0 :: (a -> b -> b) -> b -> RadixTree a -> b
foldr0 f z (RadixTree mx t) =
  let z' = Data.RadixNTree.Word8.Strict.foldr1 f z t
  in case mx of
       Just x  -> f x z'
       Nothing -> z'

foldr1 :: (a -> b -> b) -> b -> Radix1Tree a -> b
foldr1 f = go
  where
    go z t =
      case t of
        Bin _ l r   -> go (go z r) l

        Tip _ mx dx -> let z' = go z dx
                       in case mx of
                            Just x  -> f x z'
                            Nothing -> z'

        Nil         -> z



foldr0' :: (a -> b -> b) -> b -> RadixTree a -> b
foldr0' f z (RadixTree mx t) =
  let !z' = Data.RadixNTree.Word8.Strict.foldr1' f z t
  in case mx of
       Just x  -> f x z'
       Nothing -> z'

foldr1' :: (a -> b -> b) -> b -> Radix1Tree a -> b
foldr1' f = go
  where
    go !z t =
      case t of
        Bin _ l r   -> let !z' = go z r
                       in go z' l

        Tip _ mx dx -> let !z' = go z dx
                       in case mx of
                            Just x  -> f x z'
                            Nothing -> z'

        Nil         -> z



foldrWithKey0 :: (Build -> a -> b -> b) -> b -> RadixTree a -> b
foldrWithKey0 f z (RadixTree mx t) =
  let z' = foldrWithKey_ (\b arr -> f (Build $ Snoc b arr)) z t
  in case mx of
       Just x  -> f (Build Lin) x z'
       Nothing -> z'

foldrWithKey1 :: (Build1 -> a -> b -> b) -> b -> Radix1Tree a -> b
foldrWithKey1 f = foldrWithKey_ (\b arr -> f (Build1 $ b :/ arr))

{-# INLINE foldrWithKey_ #-}
foldrWithKey_ :: (Tsil ByteArray -> ByteArray -> a -> b -> b) -> b -> Radix1Tree a -> b
foldrWithKey_ f = go Lin
  where
    go b z t =
      case t of
        Bin _ l r     -> go b (go b z r) l

        Tip arr mx dx -> let z' = go (Snoc b arr) z dx
                         in case mx of
                              Just x  -> f b arr x z'
                              Nothing -> z'

        Nil           -> z



foldrWithKey0' :: (Build -> a -> b -> b) -> b -> RadixTree a -> b
foldrWithKey0' f z (RadixTree mx t) =
  let !z' = foldrWithKey'_ (\b arr -> f (Build $ Snoc b arr)) z t
  in case mx of
       Just x  -> f (Build Lin) x z'
       Nothing -> z'

foldrWithKey1' :: (Build1 -> a -> b -> b) -> b -> Radix1Tree a -> b
foldrWithKey1' f = foldrWithKey'_ (\b arr -> f (Build1 $ b :/ arr))

{-# INLINE foldrWithKey'_ #-}
foldrWithKey'_ :: (Tsil ByteArray -> ByteArray -> a -> b -> b) -> b -> Radix1Tree a -> b
foldrWithKey'_ f = go Lin
  where
    go b !z t =
      case t of
        Bin _ l r     -> let !z' = go b z r
                         in go b z' l

        Tip arr mx dx -> let !z' = go (Snoc b arr) z dx
                         in case mx of
                              Just x  -> f b arr x z'
                              Nothing -> z'

        Nil           -> z



foldMap0 :: Monoid m => (a -> m) -> RadixTree a -> m
foldMap0 f (RadixTree mx t) =
  let m = foldMap1 f t
  in case mx of
       Just x  -> f x <> m
       Nothing -> m

foldMap1 :: Monoid m => (a -> m) -> Radix1Tree a -> m
foldMap1 f = go
  where
    go t =
      case t of
        Bin _ l r   -> go l <> go r

        Tip _ mx dx -> let m = go dx
                       in case mx of
                            Nothing -> m
                            Just a  -> f a <> m

        Nil         -> mempty



foldMapWithKey0 :: Monoid m => (Build -> a -> m) -> RadixTree a -> m
foldMapWithKey0 f (RadixTree mx t) =
  let m = foldMapWithKey_ (\b arr -> f (Build $ Snoc b arr)) t
  in case mx of
       Just x  -> f (Build Lin) x <> m
       Nothing -> m

foldMapWithKey1 :: Monoid m => (Build1 -> a -> m) -> Radix1Tree a -> m
foldMapWithKey1 f = foldMapWithKey_ (\b arr -> f (Build1 $ b :/ arr))

{-# INLINE foldMapWithKey_ #-}
foldMapWithKey_
  :: Monoid m => (Tsil ByteArray -> ByteArray -> a -> m) -> Radix1Tree a -> m
foldMapWithKey_ f = go Lin
  where
    go b t =
      case t of
        Bin _ l r     -> go b l <> go b r

        Tip arr mx dx ->
          let m = go (Snoc b arr) dx
          in case mx of
               Nothing -> m
               Just a  -> f b arr a <> m

        Nil           -> mempty



traverse0 :: Applicative f => (a -> f b) -> RadixTree a -> f (RadixTree b)
traverse0 f (RadixTree mx t) =
  let dy = traverse1 f t
  in case mx of
       Just x  -> liftA2 RadixTree (Just <$> f x) dy
       Nothing -> RadixTree Nothing <$> dy

traverse1 :: Applicative f => (a -> f b) -> Radix1Tree a -> f (Radix1Tree b)
traverse1 f = go
  where
    go t =
      case t of
        Bin p l r     -> liftA2 (Bin p) (go l) (go r)

        Tip arr mx dx ->
          case mx of
            Nothing -> Tip arr Nothing <$> go dx
            Just x  -> liftA2 (Tip arr . Just) (f x) (go dx)

        Nil           -> pure Nil



traverseWithKey0 :: Applicative f => (Build -> a -> f b) -> RadixTree a -> f (RadixTree b)
traverseWithKey0 f (RadixTree mx t) =
  let dy = traverseWithKey_ (\b arr -> f (Build $ Snoc b arr)) t
  in case mx of
       Just x  -> liftA2 RadixTree (Just <$> f (Build Lin) x) dy
       Nothing -> RadixTree Nothing <$> dy

traverseWithKey1
  :: Applicative f => (Build1 -> a -> f b) -> Radix1Tree a -> f (Radix1Tree b)
traverseWithKey1 f = traverseWithKey_ (\b arr -> f (Build1 $ b :/ arr))

{-# INLINE traverseWithKey_ #-}
traverseWithKey_
  :: Applicative f
  => (Tsil ByteArray -> ByteArray -> a -> f b) -> Radix1Tree a -> f (Radix1Tree b)
traverseWithKey_ f = go Lin
  where
    go b t =
      case t of
        Bin p l r     -> liftA2 (Bin p) (go b l) (go b r)

        Tip arr mx dx ->
          let dy = go (Snoc b arr) dx
          in case mx of
               Nothing -> Tip arr Nothing <$> dy
               Just a  -> liftA2 (Tip arr . Just) (f b arr a) dy

        Nil           -> pure Nil



{-# INLINE lookup0 #-}
lookup0 :: Feed -> RadixTree a -> Maybe a
lookup0 (Feed feed) = \(RadixTree mx t) ->
  feed $ \step s ->
    case step s of
      More w z -> lookup_ step w z t
      Done     -> mx

{-# INLINE lookup1 #-}
lookup1 :: Feed1 -> Radix1Tree a -> Maybe a
lookup1 (Feed1 w feed) = feed $ \step -> lookup_ step w

{-# INLINE lookup_ #-}
lookup_ :: (x -> Step Word8 x) -> Word8 -> x -> Radix1Tree a -> Maybe a
lookup_ step = go
  where
    go !w !s t =
      case t of
        Bin p l r ->
          go w s $ if w < p
                     then l
                     else r

        Tip arr mx dx -> goarr w s 0
          where
            goarr v !z n
              | v == indexByteArray arr n =
                  let n' = n + 1
                  in if n' >= sizeofByteArray arr
                       then case step z of
                              More u z' -> go u z' dx
                              Done      -> mx

                       else case step z of
                              More u z' -> goarr u z' n'
                              Done      -> Nothing

              | otherwise = Nothing

        Nil -> Nothing



{-# INLINE find0 #-}
find0 :: a -> Feed -> RadixTree a -> a
find0 d (Feed feed) = \(RadixTree mx t) ->
  feed $ \step s ->
    case step s of
      More w z -> find_ d step w z t
      Done     -> case mx of
                    Just x  -> x
                    Nothing -> d

{-# INLINE find1 #-}
find1 :: a -> Feed1 -> Radix1Tree a -> a
find1 d (Feed1 w feed) = feed $ \step -> find_ d step w

{-# INLINE find_ #-}
find_ :: a -> (x -> Step Word8 x) -> Word8 -> x -> Radix1Tree a -> a
find_ d step = go
  where
    go !w !s t =
      case t of
        Bin p l r ->
          go w s $ if w < p
                     then l
                     else r

        Tip arr mx dx -> goarr w s 0
          where
            goarr v !z n
              | v == indexByteArray arr n =
                  let n' = n + 1
                  in if n' >= sizeofByteArray arr
                       then case step z of
                              More u z' -> go u z' dx
                              Done      -> case mx of
                                             Just x  -> x
                                             Nothing -> d

                       else case step z of
                              More u z' -> goarr u z' n'
                              Done      -> d

              | otherwise = d

        Nil -> d



{-# INLINE member0 #-}
member0 :: Feed -> RadixTree a -> Bool
member0 (Feed feed) = \(RadixTree mx t) ->
  feed $ \step s ->
    case step s of
      More w z -> member_ step w z t
      Done     -> case mx of
                    Just _  -> True
                    Nothing -> False

{-# INLINE member1 #-}
member1 :: Feed1 -> Radix1Tree a -> Bool
member1 (Feed1 w feed) = feed $ \step -> member_ step w

{-# INLINE member_ #-}
member_ :: (x -> Step Word8 x) -> Word8 -> x -> Radix1Tree a -> Bool
member_ step = go
  where
    go !w !s t =
      case t of
        Bin p l r ->
          go w s $ if w < p
                     then l
                     else r

        Tip arr mx dx -> goarr w s 0
          where
            goarr v !z n
              | v == indexByteArray arr n =
                  let n' = n + 1
                  in if n' >= sizeofByteArray arr
                       then case step z of
                              More u z' -> go u z' dx
                              Done      -> case mx of
                                             Just _  -> True
                                             Nothing -> False

                       else case step z of
                              More u z' -> goarr u z' n'
                              Done      -> False

              | otherwise = False

        Nil -> False



{-# INLINE subtree0 #-}
subtree0 :: Feed -> RadixTree a -> RadixTree a
subtree0 (Feed feed) = \t0@(RadixTree _ t) ->
  feed $ \step s ->
    case step s of
      More w z -> subtree_ step w z t
      Done     -> t0

{-# INLINE subtree1 #-}
subtree1 :: Feed1 -> Radix1Tree a -> RadixTree a
subtree1 (Feed1 w feed) = feed $ \step -> subtree_ step w

{-# INLINE subtree_ #-}
subtree_ :: (x -> Step Word8 x) -> Word8 -> x -> Radix1Tree a -> RadixTree a
subtree_ step = go
  where
    go !w !s t =
      case t of
        Bin p l r ->
          go w s $ if w < p
                     then l
                     else r

        Tip arr mx dx -> goarr w s 0
          where
            goarr v !z n
              | v == indexByteArray arr n =
                  if n + 1 >= sizeofByteArray arr
                    then case step z of
                           More u z' -> go u z' dx
                           Done      -> RadixTree mx dx

                    else case step z of
                           More u z' -> goarr u z' (n + 1)
                           Done      -> let rest = dropTrim (n + 1) arr mx dx
                                        in rest `seq` RadixTree Nothing rest

              | otherwise = RadixTree Nothing Nil

        Nil -> RadixTree Nothing Nil



{-# INLINE prefix0 #-}
prefix0 :: Feed -> RadixTree a -> RadixTree a
prefix0 (Feed feed) = \t ->
  feed $ \step s ->
    case step s of
      More w z -> RadixTree Nothing $ prefix_ step w z t
      Done     -> t

{-# INLINE prefix1 #-}
prefix1 :: Feed1 -> RadixTree a -> Radix1Tree a
prefix1 (Feed1 w feed) =
  feed $ \step -> prefix_ step w

{-# INLINE prefix_ #-}
prefix_ :: (x -> Step Word8 x) -> Word8 -> x -> RadixTree a -> Radix1Tree a
prefix_ step = \ !w !z (RadixTree mx t) ->
  case mx of
    Nothing ->
      case t of
        Bin _ _ _     -> Tip (fromStep step w z) Nothing t
        Tip arr my dy -> Tip (fromStepAppend step w z arr) my dy
        Nil           -> Nil

    Just _  -> Tip (fromStep step w z) mx t



{-# INLINE lookupL0 #-}
lookupL0 :: Openness -> Feed -> RadixTree a -> Maybe (Lookup a)
lookupL0 openness (Feed feed) (RadixTree mx t) =
  feed $ \step s ->
    case step s of
      More w z ->
        let l = lookupL_ (\b arr -> Lookup (Build $ Snoc b arr)) openness step w z t
        in case l of
             Just _  -> l
             Nothing ->
               case mx of
                 Just x  -> Just $ Lookup (Build Lin) x
                 Nothing -> Nothing

      _        ->
        case openness of
          Open   -> Nothing
          Closed -> case mx of
                      Just x  -> Just $ Lookup (Build Lin) x
                      Nothing -> Nothing

{-# INLINE lookupL1 #-}
lookupL1 :: Openness -> Feed1 -> Radix1Tree a -> Maybe (Lookup1 a)
lookupL1 openness (Feed1 w feed) =
  feed $ \step -> lookupL_ (\b arr -> Lookup1 (Build1 (b :/ arr))) openness step w

{-# INLINE lookupL_ #-}
lookupL_
  :: (Tsil ByteArray -> ByteArray -> a -> b)
  -> Openness -> (x -> Step Word8 x) -> Word8 -> x -> Radix1Tree a -> Maybe b
lookupL_ f openness step = go Lin Nothing
  where
    getMax b t =
      let !(# b', arr, a #) = unsafeLookupMaxWithKey_ b t
      in Just $! f b' arr a

    go b getL !w !s t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then go b getL w s l
                   else getL

            else if w <= upper p
                   then go b (getMax b l) w s r
                   else getMax b r

        Tip arr mx dx -> goarr w s 0
          where
            getThis = f b arr `fmap'` mx

            goarr v !z n =
              let n' = n + 1
              in case indexByteArray arr n `compare` v of
                   EQ | n' >= sizeofByteArray arr ->
                          let getL' = getThis <|> getL
                          in case step z of
                               More u z' -> go (Snoc b arr) getL' u z' dx
                               Done      ->
                                 case openness of
                                   Open   -> getL
                                   Closed -> getL'

                      | otherwise ->
                          case step z of
                            More u z' -> goarr u z' n'
                            Done      -> getL

                   LT -> case dx of
                           Nil -> getThis
                           _   -> getMax (Snoc b arr) dx

                   GT -> getL

        Nil -> getL



{-# INLINE lookupR0 #-}
lookupR0 :: Openness -> Feed -> RadixTree a -> Maybe (Lookup a)
lookupR0 openness (Feed feed) = \(RadixTree mx t) ->
  feed $ \step s ->
    case step s of
      More w z ->
        lookupR_ (\b arr -> Lookup (Build $ Snoc b arr)) openness step w z t

      _        ->
        case openness of
          Closed | Just x <- mx -> Just $ Lookup (Build Lin) x

          _      -> case t of
                      Nil -> Nothing
                      _   -> let !(# b, arr, x #) = unsafeLookupMinWithKey_ Lin t
                             in Just $! Lookup (Build $ Snoc b arr) x

{-# INLINE lookupR1 #-}
lookupR1 :: Openness -> Feed1 -> Radix1Tree a -> Maybe (Lookup1 a)
lookupR1 openness (Feed1 w feed) =
  feed $ \step -> lookupR_ (\b arr -> Lookup1 (Build1 (b :/ arr))) openness step w

{-# INLINE lookupR_ #-}
lookupR_
  :: (Tsil ByteArray -> ByteArray -> a -> b)
  -> Openness -> (x -> Step Word8 x) -> Word8 -> x -> Radix1Tree a -> Maybe b
lookupR_ f openness step = go Lin Nothing
  where
    getMin b t =
      let !(# b', arr, a #) = unsafeLookupMinWithKey_ b t
      in Just $! f b' arr a

    go b getR w !s t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then go b (getMin b r) w s l
                   else getMin b l

            else if w <= upper p
                   then go b getR w s r
                   else getR

        Tip arr mx dx -> goarr w s 0
          where
            getThis = f b arr `fmap'` mx

            getBelow =
              case dx of
                Nil -> Nothing
                _   -> getMin (Snoc b arr) dx

            goarr v !z n =
              let n' = n + 1
              in case indexByteArray arr n `compare` v of
                   EQ | n' >= sizeofByteArray arr ->
                          case step z of
                            More u z' -> go (Snoc b arr) getR u z' dx
                            Done      ->
                                  ( case openness of
                                      Open   -> getBelow
                                      Closed -> getThis <|> getBelow
                                  )
                              <|> getR

                      | otherwise ->
                          case step z of
                            More u z' -> goarr u z' n'
                            Done      -> (getThis <|> getBelow) <|> getR

                   GT -> getThis <|> getBelow

                   LT -> getR

        Nil -> getR



{-# INLINE adjustL0 #-}
adjustL0 :: (a -> a) -> Openness -> Feed -> RadixTree a -> RadixTree a
adjustL0 f openness (Feed feed) = \t0@(RadixTree mx t) ->
  feed $ \step s ->
    case step s of
      More w z -> RadixTree (f <$> mx) $ adjustL_ f openness step w z t
      Done     ->
        case openness of
          Open   -> t0
          Closed -> case mx of
                      Just x  -> RadixTree (Just $ f x) t
                      Nothing -> t0

{-# INLINE adjustL1 #-}
adjustL1 :: (a -> a) -> Openness -> Feed1 -> Radix1Tree a -> Radix1Tree a
adjustL1 f openness (Feed1 w feed) =
  feed $ \step -> adjustL_ f openness step w

{-# INLINE adjustL_ #-}
adjustL_
  :: (a -> a) -> Openness -> (x -> Step Word8 x)
  -> Word8 -> x -> Radix1Tree a -> Radix1Tree a
adjustL_ f openness step = go
  where
    go !w !s t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then Bin p (go w s l) r
                   else t

            else if w <= upper p
                   then Bin p (map1 f l) (go w s r)
                   else map1 f t

        Tip arr mx dx -> goarr w s 0
          where
            goarr v !z n =
              let n' = n + 1
              in case indexByteArray arr n `compare` v of
                   EQ | n' >= sizeofByteArray arr ->
                          case step z of
                            More u z' -> Tip arr (f <$> mx) $ go u z' dx
                            Done      ->
                              let my = case openness of
                                         Open   -> mx
                                         Closed -> f <$> mx

                              in Tip arr my dx

                      | otherwise ->
                          case step z of
                            More u z' -> goarr u z' n'
                            Done      -> t

                   LT -> map1 f t

                   GT -> t

        Nil -> Nil



{-# INLINE adjustL0' #-}
adjustL0' :: (a -> a) -> Openness -> Feed -> RadixTree a -> RadixTree a
adjustL0' f openness (Feed feed) = \t0@(RadixTree mx t) ->
  feed $ \step s ->
    case step s of
      More w z -> RadixTree (f `fmap'` mx) $ adjustL'_ f openness step w z t
      Done     ->
        case openness of
          Open   -> t0
          Closed -> case mx of
                      Just x  -> RadixTree (Just $! f x) t
                      Nothing -> t0

{-# INLINE adjustL1' #-}
adjustL1' :: (a -> a) -> Openness -> Feed1 -> Radix1Tree a -> Radix1Tree a
adjustL1' f openness (Feed1 w feed) =
  feed $ \step -> adjustL'_ f openness step w

{-# INLINE adjustL'_ #-}
adjustL'_
  :: (a -> a) -> Openness -> (x -> Step Word8 x)
  -> Word8 -> x -> Radix1Tree a -> Radix1Tree a
adjustL'_ f openness step = go
  where
    go !w !s t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then Bin p (go w s l) r
                   else t

            else if w <= upper p
                   then Bin p (map1' f l) (go w s r)
                   else map1' f t

        Tip arr mx dx -> goarr w s 0
          where
            goarr v !z n =
              let n' = n + 1
              in case indexByteArray arr n `compare` v of
                   EQ | n' >= sizeofByteArray arr ->
                          case step z of
                            More u z' -> Tip arr (f `fmap'` mx) $ go u z' dx
                            Done      ->
                              let my = case openness of
                                         Open   -> mx
                                         Closed -> f `fmap'` mx

                              in Tip arr my dx

                      | otherwise ->
                          case step z of
                            More u z' -> goarr u z' n'
                            Done      -> t

                   LT -> map1' f t

                   GT -> t

        Nil -> Nil



{-# INLINE adjustLWithKey0 #-}
adjustLWithKey0 :: (Build -> a -> a) -> Openness -> Feed -> RadixTree a -> RadixTree a
adjustLWithKey0 f openness (Feed feed) = \t0@(RadixTree mx t) ->
  feed $ \step s ->
    case step s of
      More w z -> RadixTree (f (Build Lin) <$> mx) $
                    adjustLWithKey_ (\b arr -> f (Build $ Snoc b arr)) openness step w z t
      Done     ->
        case openness of
          Open   -> t0
          Closed -> RadixTree (f (Build Lin) <$> mx) t

{-# INLINE adjustLWithKey1 #-}
adjustLWithKey1 :: (Build1 -> a -> a) -> Openness -> Feed1 -> Radix1Tree a -> Radix1Tree a
adjustLWithKey1 f openness (Feed1 w feed) =
  feed $ \step -> adjustLWithKey_ (\b arr -> f (Build1 $ b :/ arr)) openness step w

{-# INLINE adjustLWithKey_ #-}
adjustLWithKey_
  :: (Tsil ByteArray -> ByteArray -> a -> a) -> Openness -> (x -> Step Word8 x)
  -> Word8 -> x -> Radix1Tree a -> Radix1Tree a
adjustLWithKey_ f openness step = go Lin
  where
    go b !w !s t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then Bin p (go b w s l) r
                   else t

            else if w <= upper p
                   then Bin p (mapWithKey_ f b l) (go b w s r)
                   else mapWithKey_ f b t

        Tip arr mx dx -> goarr w s 0
          where
            goarr v !z n =
              let n' = n + 1
              in case indexByteArray arr n `compare` v of
                   EQ | n' >= sizeofByteArray arr ->
                          case step z of
                            More u z' -> Tip arr (f b arr <$> mx) $
                                           go (Snoc b arr) u z' dx
                            Done      ->
                              let my = case openness of
                                         Open   -> mx
                                         Closed -> f b arr <$> mx

                              in Tip arr my dx

                      | otherwise ->
                          case step z of
                            More u z' -> goarr u z' n'
                            Done      -> t

                   LT -> mapWithKey_ f b t

                   GT -> t

        Nil -> Nil



{-# INLINE adjustLWithKey0' #-}
adjustLWithKey0' :: (Build -> a -> a) -> Openness -> Feed -> RadixTree a -> RadixTree a
adjustLWithKey0' f openness (Feed feed) = \t0@(RadixTree mx t) ->
  feed $ \step s ->
    case step s of
      More w z ->
        RadixTree (f (Build Lin) `fmap'` mx) $
          adjustLWithKey'_ (\b arr -> f (Build $ Snoc b arr)) openness step w z t

      Done     ->
        case openness of
          Open   -> t0
          Closed -> case mx of
                      Just x  -> RadixTree (Just $! f (Build Lin) x) t
                      Nothing -> t0

{-# INLINE adjustLWithKey1' #-}
adjustLWithKey1'
  :: (Build1 -> a -> a) -> Openness -> Feed1 -> Radix1Tree a -> Radix1Tree a
adjustLWithKey1' f openness (Feed1 w feed) =
  feed $ \step -> adjustLWithKey'_ (\b arr -> f (Build1 $ b :/ arr)) openness step w

{-# INLINE adjustLWithKey'_ #-}
adjustLWithKey'_
  :: (Tsil ByteArray -> ByteArray -> a -> a) -> Openness -> (x -> Step Word8 x)
  -> Word8 -> x -> Radix1Tree a -> Radix1Tree a
adjustLWithKey'_ f openness step = go Lin
  where
    go b !w !s t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then Bin p (go b w s l) r
                   else t

            else if w <= upper p
                   then Bin p (mapWithKey'_ f b l) (go b w s r)
                   else mapWithKey'_ f b t

        Tip arr mx dx -> goarr w s 0
          where
            goarr v !z n =
              let n' = n + 1
              in case indexByteArray arr n `compare` v of
                   EQ | n' >= sizeofByteArray arr ->
                          case step z of
                            More u z' -> Tip arr (f b arr `fmap'` mx) $
                                           go (Snoc b arr) u z' dx
                            Done      ->
                              let my = case openness of
                                         Open   -> mx
                                         Closed -> f b arr `fmap'` mx

                              in Tip arr my dx

                      | otherwise ->
                          case step z of
                            More u z' -> goarr u z' n'
                            Done      -> t

                   LT -> mapWithKey'_ f b t

                   GT -> t

        Nil -> Nil



{-# INLINE adjustR0 #-}
adjustR0 :: (a -> a) -> Openness -> Feed -> RadixTree a -> RadixTree a
adjustR0 f openness (Feed feed) = \(RadixTree mx t) ->
  feed $ \step s ->
    case step s of
      More w z -> RadixTree mx $ adjustR_ f openness step w z t
      Done     ->
        let my = case openness of
                   Open   -> mx
                   Closed -> f <$> mx

        in RadixTree my (map1 f t)

{-# INLINE adjustR1 #-}
adjustR1 :: (a -> a) -> Openness -> Feed1 -> Radix1Tree a -> Radix1Tree a
adjustR1 f openness (Feed1 w feed) =
  feed $ \step -> adjustR_ f openness step w

{-# INLINE adjustR_ #-}
adjustR_
  :: (a -> a) -> Openness -> (x -> Step Word8 x)
  -> Word8 -> x -> Radix1Tree a -> Radix1Tree a
adjustR_ f openness step = go
  where
    go !w !s t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then Bin p (go w s l) (map1 f r)
                   else map1 f t

            else if w <= upper p
                   then Bin p l (go w s r)
                   else t

        Tip arr mx dx -> goarr w s 0
          where
            goarr v !z n =
              let n' = n + 1
              in case indexByteArray arr n `compare` v of
                   EQ | n' >= sizeofByteArray arr ->
                          case step z of
                            More u z' -> Tip arr mx $ go u z' dx
                            Done      ->
                              let my = case openness of
                                         Open   -> mx
                                         Closed -> f <$> mx

                              in Tip arr my $ map1 f dx

                      | otherwise ->
                          case step z of
                            More u z' -> goarr u z' n'
                            Done      -> map1 f t

                   GT -> map1 f t

                   LT -> t

        Nil -> Nil



{-# INLINE adjustR0' #-}
adjustR0' :: (a -> a) -> Openness -> Feed -> RadixTree a -> RadixTree a
adjustR0' f openness (Feed feed) = \(RadixTree mx t) ->
  feed $ \step s ->
    case step s of
      More w z -> RadixTree mx $ adjustR'_ f openness step w z t
      Done     ->
        let my = case openness of
                   Open   -> mx
                   Closed -> f `fmap'` mx

        in RadixTree my (map1' f t)

{-# INLINE adjustR1' #-}
adjustR1' :: (a -> a) -> Openness -> Feed1 -> Radix1Tree a -> Radix1Tree a
adjustR1' f openness (Feed1 w feed) =
  feed $ \step -> adjustR'_ f openness step w

{-# INLINE adjustR'_ #-}
adjustR'_
  :: (a -> a) -> Openness -> (x -> Step Word8 x)
  -> Word8 -> x -> Radix1Tree a -> Radix1Tree a
adjustR'_ f openness step = go
  where
    go !w !s t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then Bin p (go w s l) (map1' f r)
                   else map1' f t

            else if w <= upper p
                   then Bin p l (go w s r)
                   else t

        Tip arr mx dx -> goarr w s 0
          where
            goarr v !z n =
              let n' = n + 1
              in case indexByteArray arr n `compare` v of
                   EQ | n' >= sizeofByteArray arr ->
                          case step z of
                            More u z' -> Tip arr mx $ go u z' dx
                            Done      ->
                              let my = case openness of
                                         Open   -> mx
                                         Closed -> f `fmap'` mx

                              in Tip arr my $ map1' f dx

                      | otherwise ->
                          case step z of
                            More u z' -> goarr u z' n'
                            Done      -> map1 f t

                   GT -> map1' f t

                   LT -> t

        Nil -> Nil



{-# INLINE adjustRWithKey0 #-}
adjustRWithKey0 :: (Build -> a -> a) -> Openness -> Feed -> RadixTree a -> RadixTree a
adjustRWithKey0 f openness (Feed feed) = \(RadixTree mx t) ->
  feed $ \step s ->
    case step s of
      More w z ->
       RadixTree mx $
         adjustRWithKey_ (\b arr -> f (Build $ Snoc b arr)) openness step w z t

      Done     ->
        let my = case openness of
                   Open   -> mx
                   Closed -> f (Build Lin) <$> mx

        in RadixTree my $ mapWithKey_ (\b arr -> f (Build $ Snoc b arr)) Lin t

{-# INLINE adjustRWithKey1 #-}
adjustRWithKey1 :: (Build1 -> a -> a) -> Openness -> Feed1 -> Radix1Tree a -> Radix1Tree a
adjustRWithKey1 f openness (Feed1 w feed) =
  feed $ \step -> adjustRWithKey_ (\b arr -> f (Build1 $ b :/ arr)) openness step w

{-# INLINE adjustRWithKey_ #-}
adjustRWithKey_
  :: (Tsil ByteArray -> ByteArray -> a -> a) -> Openness -> (x -> Step Word8 x)
  -> Word8 -> x -> Radix1Tree a -> Radix1Tree a
adjustRWithKey_ f openness step = go Lin
  where
    go b !w !s t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then Bin p (go b w s l) (mapWithKey_ f b r)
                   else mapWithKey_ f b t

            else if w <= upper p
                   then Bin p l (go b w s r)
                   else t

        Tip arr mx dx -> goarr w s 0
          where
            goarr v !z n =
              let n' = n + 1
              in case indexByteArray arr n `compare` v of
                   EQ | n' >= sizeofByteArray arr ->
                          case step z of
                            More u z' -> Tip arr mx $ go (Snoc b arr) u z' dx
                            Done      ->
                              let my = case openness of
                                         Open   -> mx
                                         Closed -> f b arr <$> mx

                              in Tip arr my $ mapWithKey_ f (Snoc b arr) dx

                      | otherwise ->
                          case step z of
                            More u z' -> goarr u z' n'
                            Done      -> mapWithKey_ f b t

                   GT -> mapWithKey_ f b t

                   LT -> t

        Nil -> Nil



{-# INLINE adjustRWithKey0' #-}
adjustRWithKey0' :: (Build -> a -> a) -> Openness -> Feed -> RadixTree a -> RadixTree a
adjustRWithKey0' f openness (Feed feed) = \(RadixTree mx t) ->
  feed $ \step s ->
    case step s of
      More w z ->
        RadixTree mx $
          adjustRWithKey'_ (\b arr -> f (Build $ Snoc b arr)) openness step w z t

      Done     ->
        let my = case openness of
                   Open   -> mx
                   Closed -> f (Build Lin) `fmap'` mx

        in RadixTree my $ mapWithKey'_ (\b arr -> f (Build $ Snoc b arr)) Lin t

{-# INLINE adjustRWithKey1' #-}
adjustRWithKey1'
  :: (Build1 -> a -> a) -> Openness -> Feed1 -> Radix1Tree a -> Radix1Tree a
adjustRWithKey1' f openness (Feed1 w feed) =
  feed $ \step -> adjustRWithKey'_ (\b arr -> f (Build1 $ b :/ arr)) openness step w

{-# INLINE adjustRWithKey'_ #-}
adjustRWithKey'_
  :: (Tsil ByteArray -> ByteArray -> a -> a) -> Openness -> (x -> Step Word8 x)
  -> Word8 -> x -> Radix1Tree a -> Radix1Tree a
adjustRWithKey'_ f openness step = go Lin
  where
    go b !w !s t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then Bin p (go b w s l) (mapWithKey'_ f b r)
                   else mapWithKey'_ f b t

            else if w <= upper p
                   then Bin p l (go b w s r)
                   else t

        Tip arr mx dx -> goarr w s 0
          where
            goarr v !z n =
              let n' = n + 1
              in case indexByteArray arr n `compare` v of
                   EQ | n' >= sizeofByteArray arr ->
                          case step z of
                            More u z' -> Tip arr mx $ go (Snoc b arr) u z' dx
                            Done      ->
                              let my = case openness of
                                         Open   -> mx
                                         Closed -> f b arr `fmap'` mx

                              in Tip arr my $ mapWithKey'_ f (Snoc b arr) dx

                      | otherwise ->
                          case step z of
                            More u z' -> goarr u z' n'
                            Done      -> mapWithKey'_ f b t

                   GT -> mapWithKey'_ f b t

                   LT -> t

        Nil -> Nil



{-# INLINE updateL0 #-}
updateL0 :: (a -> Maybe a) -> Openness -> Feed -> RadixTree a -> RadixTree a
updateL0 f openness (Feed feed) = \t0@(RadixTree mx t) ->
  feed $ \step s ->
    case step s of
      More w z -> RadixTree (f =<< mx) $ updateL_ f openness step w z t
      Done     ->
        case openness of
          Open   -> t0
          Closed -> RadixTree (f =<< mx) t

{-# INLINE updateL1 #-}
updateL1 :: (a -> Maybe a) -> Openness -> Feed1 -> Radix1Tree a -> Radix1Tree a
updateL1 f openness (Feed1 w feed) =
  feed $ \step -> updateL_ f openness step w

{-# INLINE updateL_ #-}
updateL_
  :: (a -> Maybe a) -> Openness -> (x -> Step Word8 x)
  -> Word8 -> x -> Radix1Tree a -> Radix1Tree a
updateL_ f openness step = go
  where
    go !w !s t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then rebinL p (go w s l) r
                   else t

            else if w <= upper p
                   then rebin p (mapMaybe1 f l) (go w s r)
                   else mapMaybe1 f t

        Tip arr mx dx -> goarr w s 0
          where
            goarr v !z n =
              let n' = n + 1
              in case indexByteArray arr n `compare` v of
                   EQ | n' >= sizeofByteArray arr ->
                          case step z of
                            More u z' -> retip arr (f =<< mx) $ go u z' dx
                            Done      ->
                              let my = case openness of
                                         Open   -> mx
                                         Closed -> f =<< mx

                              in retip arr my dx

                      | otherwise ->
                          case step z of
                            More u z' -> goarr u z' n'
                            Done      -> t

                   LT -> mapMaybe1 f t

                   GT -> t

        Nil -> Nil



{-# INLINE updateLWithKey0 #-}
updateLWithKey0
  :: (Build -> a -> Maybe a) -> Openness -> Feed -> RadixTree a -> RadixTree a
updateLWithKey0 f openness (Feed feed) = \t0@(RadixTree mx t) ->
  feed $ \step s ->
    case step s of
      More w z ->
        RadixTree (f (Build Lin) =<< mx) $
          updateLWithKey_ (\b arr -> f (Build $ Snoc b arr)) openness step w z t

      Done     ->
        case openness of
          Open   -> t0
          Closed -> RadixTree (f (Build Lin) =<< mx) t

{-# INLINE updateLWithKey1 #-}
updateLWithKey1
  :: (Build1 -> a -> Maybe a) -> Openness -> Feed1 -> Radix1Tree a -> Radix1Tree a
updateLWithKey1 f openness (Feed1 w feed) =
  feed $ \step -> updateLWithKey_ (\b arr -> f (Build1 $ b :/ arr)) openness step w

{-# INLINE updateLWithKey_ #-}
updateLWithKey_
  :: (Tsil ByteArray -> ByteArray -> a -> Maybe a) -> Openness -> (x -> Step Word8 x)
  -> Word8 -> x -> Radix1Tree a -> Radix1Tree a
updateLWithKey_ f openness step = go Lin
  where
    go b !w !s t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then rebinL p (go b w s l) r
                   else t

            else if w <= upper p
                   then rebin p (mapMaybeWithKey_ f b l) (go b w s r)
                   else mapMaybeWithKey_ f b t

        Tip arr mx dx -> goarr w s 0
          where
            goarr v !z n =
              let n' = n + 1
              in case indexByteArray arr n `compare` v of
                   EQ | n' >= sizeofByteArray arr ->
                          case step z of
                            More u z' -> retip arr (f b arr =<< mx) $
                                           go (Snoc b arr) u z' dx
                            Done      ->
                              let my = case openness of
                                         Open   -> mx
                                         Closed -> f b arr =<< mx

                              in retip arr my dx

                      | otherwise ->
                          case step z of
                            More u z' -> goarr u z' n'
                            Done      -> t

                   LT -> mapMaybeWithKey_ f b t

                   GT -> t

        Nil -> Nil



{-# INLINE updateR0 #-}
updateR0 :: (a -> Maybe a) -> Openness -> Feed -> RadixTree a -> RadixTree a
updateR0 f openness (Feed feed) (RadixTree mx t) =
  feed $ \step s ->
    case step s of
      More w z -> RadixTree mx $ updateR_ f openness step w z t
      Done     ->
        let my = case openness of
                   Open   -> mx
                   Closed -> f =<< mx

        in RadixTree my (mapMaybe1 f t)

{-# INLINE updateR1 #-}
updateR1 :: (a -> Maybe a) -> Openness -> Feed1 -> Radix1Tree a -> Radix1Tree a
updateR1 f openness (Feed1 w feed) =
  feed $ \step -> updateR_ f openness step w

{-# INLINE updateR_ #-}
updateR_
  :: (a -> Maybe a) -> Openness -> (x -> Step Word8 x)
  -> Word8 -> x -> Radix1Tree a -> Radix1Tree a
updateR_ f openness step = go
  where
    go !w !s t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then rebin p (go w s l) (mapMaybe1 f r)
                   else mapMaybe1 f t

            else if w <= upper p
                   then rebinR p l (go w s r)
                   else t

        Tip arr mx dx -> goarr w s 0
          where
            goarr v !z n =
              let n' = n + 1
              in case indexByteArray arr n `compare` v of
                   EQ | n' >= sizeofByteArray arr ->
                          case step z of
                            More u z' -> retip arr mx $ go u z' dx
                            Done      ->
                              let my = case openness of
                                         Open   -> mx
                                         Closed -> f =<< mx

                              in retip arr my $ mapMaybe1 f dx

                      | otherwise ->
                          case step z of
                            More u z' -> goarr u z' n'
                            Done      -> mapMaybe1 f t

                   GT -> mapMaybe1 f t

                   LT -> t

        Nil -> Nil



{-# INLINE updateRWithKey0 #-}
updateRWithKey0
  :: (Build -> a -> Maybe a) -> Openness -> Feed -> RadixTree a -> RadixTree a
updateRWithKey0 f openness (Feed feed) (RadixTree mx t) =
  feed $ \step s ->
    case step s of
      More w z ->
        RadixTree mx $
          updateRWithKey_ (\b arr -> f (Build $ Snoc b arr)) openness step w z t

      Done     ->
        let my = case openness of
                   Open   -> mx
                   Closed -> f (Build Lin) =<< mx

        in RadixTree my (mapMaybeWithKey_ (\b arr -> f (Build $ Snoc b arr)) Lin t)

{-# INLINE updateRWithKey1 #-}
updateRWithKey1
  :: (Build1 -> a -> Maybe a) -> Openness -> Feed1 -> Radix1Tree a -> Radix1Tree a
updateRWithKey1 f openness (Feed1 w feed) =
  feed $ \step -> updateRWithKey_ (\b arr -> f (Build1 $ b :/ arr)) openness step w

{-# INLINE updateRWithKey_ #-}
updateRWithKey_
  :: (Tsil ByteArray -> ByteArray -> a -> Maybe a) -> Openness -> (x -> Step Word8 x)
  -> Word8 -> x -> Radix1Tree a -> Radix1Tree a
updateRWithKey_ f openness step = go Lin
  where
    go b !w !s t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then rebin p (go b w s l) (mapMaybeWithKey_ f b r)
                   else mapMaybeWithKey_ f b t

            else if w <= upper p
                   then rebinR p l (go b w s r)
                   else t

        Tip arr mx dx -> goarr w s 0
          where
            goarr v !z n =
              let n' = n + 1
              in case indexByteArray arr n `compare` v of
                   EQ | n' >= sizeofByteArray arr ->
                          case step z of
                            More u z' -> retip arr mx $ go (Snoc b arr) u z' dx
                            Done      ->
                              let my = case openness of
                                         Open   -> mx
                                         Closed -> f b arr =<< mx

                              in retip arr my $ mapMaybeWithKey_ f (Snoc b arr) dx

                      | otherwise ->
                          case step z of
                            More u z' -> goarr u z' n'
                            Done      -> mapMaybeWithKey_ f b t

                   GT -> mapMaybeWithKey_ f b t

                   LT -> t

        Nil -> Nil



{-# INLINE takeL0 #-}
takeL0 :: Openness -> Feed -> RadixTree a -> RadixTree a
takeL0 openness (Feed feed) = \(RadixTree mx t) ->
  feed $ \step s ->
    case step s of
      More w z -> RadixTree mx $ takeL_ openness step w z t
      Done     ->
        let my = case openness of
                   Open   -> Nothing
                   Closed -> mx

        in RadixTree my Nil

{-# INLINE takeL1 #-}
takeL1 :: Openness -> Feed1 -> Radix1Tree a -> Radix1Tree a
takeL1 openness (Feed1 w0 feed) = feed $ \step -> takeL_ openness step w0

{-# INLINE takeL_ #-}
takeL_ :: Openness -> (x -> Step Prefix x) -> Prefix -> x -> Radix1Tree a -> Radix1Tree a
takeL_ openness step = go
  where
    go !w !s t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then go w s l
                   else Nil

            else if w <= upper p
                   then rebinR p l (go w s r)
                   else t

        Tip arr mx dx -> goarr w s 0
          where
            goarr v !z n =
              let n' = n + 1
              in case indexByteArray arr n `compare` v of
                   EQ | n' >= sizeofByteArray arr ->
                          case step z of
                            More u z' -> retip arr mx $ go u z' dx
                            Done      ->
                              case openness of
                                Open   -> Nil
                                Closed -> retip arr mx Nil

                      | otherwise ->
                          case step z of
                            More u z' -> goarr u z' n'
                            Done      -> Nil

                   LT -> t

                   GT -> Nil

        Nil -> Nil



{-# INLINE takeR0 #-}
takeR0 :: Openness -> Feed -> RadixTree a -> RadixTree a
takeR0 openness (Feed feed) (RadixTree mx t) =
  feed $ \step s ->
    case step s of
      More w z -> RadixTree Nothing $ takeR_ openness step w z t
      Done     ->
        let my = case openness of
                   Open   -> Nothing
                   Closed -> mx

        in RadixTree my t

{-# INLINE takeR1 #-}
takeR1 :: Openness -> Feed1 -> Radix1Tree a -> Radix1Tree a
takeR1 openness (Feed1 w0 feed) = feed $ \step -> takeR_ openness step w0

{-# INLINE takeR_ #-}
takeR_ :: Openness -> (x -> Step Prefix x) -> Prefix -> x -> Radix1Tree a -> Radix1Tree a
takeR_ openness step = go
  where
    go !w !s t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then rebinL p (go w s l) r
                   else t

            else if w <= upper p
                   then go w s r
                   else Nil

        Tip arr mx dx -> goarr w s 0
          where
            goarr v !z n =
              let n' = n + 1
              in case indexByteArray arr n `compare` v of
                   EQ | n' >= sizeofByteArray arr ->
                          case step z of
                            More u z' -> retip arr Nothing $ go u z' dx
                            Done      ->
                              let my = case openness of
                                         Open   -> Nothing
                                         Closed -> mx

                              in retip arr my dx

                      | otherwise ->
                          case step z of
                            More u z' -> goarr u z' n'
                            Done      -> t

                   GT -> t

                   LT -> Nil

        Nil -> Nil



type UBin a = (# Prefix, Radix1Tree a, Radix1Tree a #)

type UTip a = (# Key, Int, ByteArray, Maybe a, Radix1Tree a #)



union0 :: RadixTree a -> RadixTree a -> RadixTree a
union0 (RadixTree mA tA) (RadixTree mB tB) = RadixTree (mA <|> mB) (union1 tA tB)

union1 :: Radix1Tree a -> Radix1Tree a -> Radix1Tree a
union1 = anyAny
  where
    anyAny tA tB =
      case tA of
        Bin pA lA rA    -> binAny (# pA, lA, rA #) tA tB

        Tip arrA mA dA  -> let !wA = indexByteArray arrA 0
                           in tipAny (# wA, 0, arrA, mA, dA #) tA tB

        Nil             -> tB

    tipAny uA@(# _, nA, arrA, mA, dA #) tA tB =
      case tB of
        Bin pB lB rB    -> tipBin uA tA (# pB, lB, rB #) tB

        Tip arrB mB dB  -> let !wB = indexByteArray arrB 0

                               uB = (# wB, 0, arrB, mB, dB #)

                               !lenA = sizeofByteArray arrA - nA

                               !lenB = sizeofByteArray arrB

                           in if lenB > lenA
                                then tipTip uA tA uB tB lenA
                                else tipTip uB tB uA tA lenB

        Nil             | nA == 0   -> tA
                        | otherwise -> Tip (dropByteArray nA arrA) mA dA

    tipTip (# wA0, nA, arrA, mA, dA #) tA (# wB0, nB, arrB, mB, dB #) tB len =
      go wA0 wB0 1
      where
        go wA wB !o
          | wA == wB  =
              let !nB' = nB + o
                  !wB' = indexByteArray arrB nB'

              in if o >= len
                   then let !arrA' | nA == 0   = arrA
                                   | otherwise = dropByteArray nA arrA

                        in if nB' == sizeofByteArray arrB
                             then Tip arrA' (mA <|> mB) (anyAny dA dB)
                             else Tip arrA' mA $
                                    tipAny (# wB', nB', arrB, mB, dB #) tB dA

                   else let !nA' = nA + o
                            !wA' = indexByteArray arrA nA'

                        in go wA' wB' (o + 1)

          | o == 1 =
              let !tA' | nA == 0   = tA
                       | otherwise = Tip (dropByteArray nA arrA) mA dA

                  !tB' | nB == 0   = tB
                       | otherwise = Tip (dropByteArray nB arrB) mB dB

              in join wA tA' wB tB'

          | otherwise =
              let !o' = o - 1

                  !(# !arrC, !arrA' #) = splitByteArray nA o' arrA

                  !arrB' = dropByteArray (nB + o') arrB

              in Tip arrC Nothing $ join wA (Tip arrA' mA dA)
                                         wB (Tip arrB' mB dB)

    binAny uA tA tB =
      case tB of
        Bin pB lB rB    -> binBin uA tA (# pB, lB, rB #) tB

        Tip arrB mB dB  -> let !wB = indexByteArray arrB 0
                           in tipBin (# wB, 0, arrB, mB, dB #) tB uA tA

        Nil             -> tA

    tipBin uA@(# wA, nA, arrA, mA, dA #) tA (# pB, lB, rB #) tB
      | beyond pB wA = let !tA' | nA == 0   = tA
                                | otherwise = Tip (dropByteArray nA arrA) mA dA

                       in join wA tA' pB tB

      | wA < pB      = Bin pB (tipAny uA tA lB) rB
      | otherwise    = Bin pB lB (tipAny uA tA rB)

    binBin uA@(# pA, lA, rA #) tA uB@(# pB, lB, rB #) tB =
      let {-# NOINLINE no #-}
          no = join pA tA pB tB

      in case Prelude.compare pA pB of
           EQ                  -> Bin pA (anyAny lA lB) (anyAny rA rB)

           LT | pB <= upper pA -> Bin pA lA (binAny uB tB rA)
              | pA >= lower pB -> Bin pB (binAny uA tA lB) rB
              | otherwise      -> no

           GT | pA <= upper pB -> Bin pB lB (binAny uA tA rB)
              | pB >= lower pA -> Bin pA (binAny uB tB lA) rA
              | otherwise      -> no



unionL0 :: RadixTree a -> RadixTree a -> RadixTree a
unionL0 (RadixTree mA tA) (RadixTree mB tB) = RadixTree (mA <|> mB) (unionL1 tA tB)

unionL1 :: Radix1Tree a -> Radix1Tree a -> Radix1Tree a
unionL1 =
  union_ $ \s a b ->
    let !(# c #) = case s of
                     L -> (# a #)
                     R -> (# b #)
    in Just c


unionWith0' :: (a -> a -> a) -> RadixTree a -> RadixTree a -> RadixTree a
unionWith0' f (RadixTree mA tA) (RadixTree mB tB) =
  let mC = case mA of
             Just a  -> case mB of
                          Just b  -> Just $! f a b
                          Nothing -> mA

             Nothing -> mB

  in RadixTree mC (unionWith1' f tA tB)

unionWith1' :: (a -> a -> a) -> Radix1Tree a -> Radix1Tree a -> Radix1Tree a
unionWith1' f =
  union_ $ \s a b ->
    Just $! case s of
              L -> f a b
              R -> f b a



{-# INLINE union_ #-}
union_
  :: (forall x y. S x y a a -> x -> y -> Maybe a)
  -> Radix1Tree a
  -> Radix1Tree a
  -> Radix1Tree a
union_ f = anyAny L
  where
    anyAny s tA tB =
      case tA of
        Bin pA lA rA    -> binAny s (# pA, lA, rA #) tA tB

        Tip arrA mA dA  -> let !wA = indexByteArray arrA 0
                           in tipAny s (# wA, 0, arrA, mA, dA #) tA tB

        Nil             -> tB

    tipAny s uA@(# _, nA, arrA, mA, dA #) tA tB =
      case tB of
        Bin pB lB rB    -> tipBin s uA tA (# pB, lB, rB #) tB

        Tip arrB mB dB  -> let !wB = indexByteArray arrB 0

                               uB = (# wB, 0, arrB, mB, dB #)

                               !lenA = sizeofByteArray arrA - nA

                               !lenB = sizeofByteArray arrB

                           in if lenB > lenA
                                then tipTip s uA tA uB tB lenA

                                else let !(# s' #) = other s
                                     in tipTip s' uB tB uA tA lenB

        Nil             | nA == 0   -> tA
                        | otherwise -> Tip (dropByteArray nA arrA) mA dA

    tipTip s (# wA0, nA, arrA, mA, dA #) tA (# wB0, nB, arrB, mB, dB #) tB len =
      go wA0 wB0 1
      where
        go wA wB !o
          | wA == wB  =
              let !nB' = nB + o
                  !wB' = indexByteArray arrB nB'

              in if o >= len
                   then let !arrA' | nA == 0   = arrA
                                   | otherwise = dropByteArray nA arrA

                        in if nB' == sizeofByteArray arrB
                             then let mC = case mA of
                                             Just a  -> case mB of
                                                          Just b  -> f s a b
                                                          Nothing -> mA

                                             Nothing -> mB

                                  in Tip arrA' mC (anyAny s dA dB)

                             else Tip arrA' mA $
                                    let !(# s' #) = other s
                                    in tipAny s' (# wB', nB', arrB, mB, dB #) tB dA

                   else let !nA' = nA + o
                            !wA' = indexByteArray arrA nA'

                        in go wA' wB' (o + 1)

          | o == 1 =
              let !tA' | nA == 0   = tA
                       | otherwise = Tip (dropByteArray nA arrA) mA dA

                  !tB' | nB == 0   = tB
                       | otherwise = Tip (dropByteArray nB arrB) mB dB

              in join wA tA' wB tB'

          | otherwise =
              let !o' = o - 1

                  !(# !arrC, !arrA' #) = splitByteArray nA o' arrA

                  !arrB' = dropByteArray (nB + o') arrB

              in Tip arrC Nothing $ join wA (Tip arrA' mA dA)
                                         wB (Tip arrB' mB dB)

    binAny s uA tA tB =
      case tB of
        Bin pB lB rB    -> binBin s uA tA (# pB, lB, rB #) tB

        Tip arrB mB dB  -> let !(# s' #) = other s

                               !wB = indexByteArray arrB 0

                           in tipBin s' (# wB, 0, arrB, mB, dB #) tB uA tA

        Nil             -> tA

    tipBin s uA@(# wA, nA, arrA, mA, dA #) tA (# pB, lB, rB #) tB
      | beyond pB wA = let !tA' | nA == 0   = tA
                                | otherwise = Tip (dropByteArray nA arrA) mA dA

                       in join wA tA' pB tB

      | wA < pB      = Bin pB (tipAny s uA tA lB) rB
      | otherwise    = Bin pB lB (tipAny s uA tA rB)

    binBin s uA@(# pA, lA, rA #) tA uB@(# pB, lB, rB #) tB =
      let {-# NOINLINE no #-}
          no = join pA tA pB tB

      in case Prelude.compare pA pB of
           EQ                  -> Bin pA (anyAny s lA lB) (anyAny s rA rB)

           LT | pB <= upper pA -> let !(# s' #) = other s
                                  in Bin pA lA (binAny s' uB tB rA)
              | pA >= lower pB -> Bin pB (binAny s uA tA lB) rB
              | otherwise      -> no

           GT | pA <= upper pB -> Bin pB lB (binAny s uA tA rB)
              | pB >= lower pA -> let !(# s' #) = other s
                                  in Bin pA (binAny s' uB tB lA) rA
              | otherwise      -> no




unionWithKey0' :: (Build -> a -> a -> a) -> RadixTree a -> RadixTree a -> RadixTree a
unionWithKey0' f (RadixTree mA tA) (RadixTree mB tB) =
  let mC = case mA of
             Just a  -> case mB of
                          Just b  -> Just $! f (Build Lin) a b
                          Nothing -> mA

             Nothing -> mB

  in RadixTree mC $ unionWithKey_
                      ( \s b arr vA vB ->
                           Just $! let b0 = Build $ Snoc b arr
                                   in case s of
                                        L -> f b0 vA vB
                                        R -> f b0 vB vA
                      )
                      tA tB

unionWithKey1' :: (Build1 -> a -> a -> a) -> Radix1Tree a -> Radix1Tree a -> Radix1Tree a
unionWithKey1' f =
  unionWithKey_ $ \s b arr vA vB ->
    Just $! let b1 = Build1 $ b :/ arr
            in case s of
                 L -> f b1 vA vB
                 R -> f b1 vB vA

{-# INLINE unionWithKey_ #-}
unionWithKey_
  :: (forall x y. S x y a a -> Tsil ByteArray -> ByteArray -> x -> y -> Maybe a)
  -> Radix1Tree a
  -> Radix1Tree a
  -> Radix1Tree a
unionWithKey_ f = anyAny L Lin
  where
    anyAny s b tA tB =
      case tA of
        Bin pA lA rA    -> binAny s b (# pA, lA, rA #) tA tB

        Tip arrA mA dA  -> let !wA = indexByteArray arrA 0
                           in tipAny s b (# wA, 0, arrA, mA, dA #) tA tB

        Nil             -> tB

    tipAny s b uA@(# _, nA, arrA, mA, dA #) tA tB =
      case tB of
        Bin pB lB rB    -> tipBin s b uA tA (# pB, lB, rB #) tB

        Tip arrB mB dB  -> let !wB = indexByteArray arrB 0

                               uB = (# wB, 0, arrB, mB, dB #)

                               !lenA = sizeofByteArray arrA - nA

                               !lenB = sizeofByteArray arrB

                           in if lenB > lenA
                                then tipTip s b uA tA uB tB lenA

                                else let !(# s' #) = other s
                                     in tipTip s' b uB tB uA tA lenB

        Nil             | nA == 0   -> tA
                        | otherwise -> Tip (dropByteArray nA arrA) mA dA

    tipTip s b (# wA0, nA, arrA, mA, dA #) tA (# wB0, nB, arrB, mB, dB #) tB len =
      go wA0 wB0 1
      where
        go wA wB !o
          | wA == wB  =
              let !nB' = nB + o
                  !wB' = indexByteArray arrB nB'

              in if o >= len
                   then let !arrA' | nA == 0   = arrA
                                   | otherwise = dropByteArray nA arrA

                        in if nB' == sizeofByteArray arrB
                             then let mC =
                                        case mA of
                                          Just xA ->
                                            case mB of
                                              Just xB -> f s b arrA' xA xB
                                              Nothing -> mA

                                          Nothing -> mB

                                  in Tip arrA' mC (anyAny s (Snoc b arrA') dA dB)

                             else Tip arrA' mA $
                                    let !(# s' #) = other s
                                    in tipAny s' (Snoc b arrA')
                                         (# wB', nB', arrB, mB, dB #) tB dA

                   else let !nA' = nA + o
                            !wA' = indexByteArray arrA nA'

                        in go wA' wB' (o + 1)

          | o == 1 =
              let !tA' | nA == 0   = tA
                       | otherwise = Tip (dropByteArray nA arrA) mA dA

                  !tB' | nB == 0   = tB
                       | otherwise = Tip (dropByteArray nB arrB) mB dB

              in join wA tA' wB tB'

          | otherwise =
              let !o' = o - 1

                  !(# !arrC, !arrA' #) = splitByteArray nA o' arrA

                  !arrB' = dropByteArray (nB + o') arrB

              in Tip arrC Nothing $ join wA (Tip arrA' mA dA)
                                         wB (Tip arrB' mB dB)

    binAny s b uA tA tB =
      case tB of
        Bin pB lB rB    -> binBin s b uA tA (# pB, lB, rB #) tB

        Tip arrB mB dB  -> let !(# s' #) = other s

                               !wB = indexByteArray arrB 0

                           in tipBin s' b (# wB, 0, arrB, mB, dB #) tB uA tA

        Nil             -> tA

    tipBin s b uA@(# wA, nA, arrA, mA, dA #) tA (# pB, lB, rB #) tB
      | beyond pB wA = let !tA' | nA == 0   = tA
                                | otherwise = Tip (dropByteArray nA arrA) mA dA

                       in join wA tA' pB tB

      | wA < pB      = Bin pB (tipAny s b uA tA lB) rB
      | otherwise    = Bin pB lB (tipAny s b uA tA rB)

    binBin s b uA@(# pA, lA, rA #) tA uB@(# pB, lB, rB #) tB =
      let {-# NOINLINE no #-}
          no = join pA tA pB tB

      in case Prelude.compare pA pB of
           EQ                  -> Bin pA (anyAny s b lA lB) (anyAny s b rA rB)

           LT | pB <= upper pA -> let !(# s' #) = other s
                                  in Bin pA lA (binAny s' b uB tB rA)
              | pA >= lower pB -> Bin pB (binAny s b uA tA lB) rB
              | otherwise      -> no

           GT | pA <= upper pB -> Bin pB lB (binAny s b uA tA rB)
              | pB >= lower pA -> let !(# s' #) = other s
                                  in Bin pA (binAny s' b uB tB lA) rA
              | otherwise      -> no



difference0 :: RadixTree a -> RadixTree b -> RadixTree a
difference0 (RadixTree mA tA) (RadixTree mB tB) =
  let mC = case mB of
             Just _  -> Nothing
             Nothing -> mA

  in RadixTree mC $ difference1 tA tB

difference1 :: Radix1Tree a -> Radix1Tree b -> Radix1Tree a
difference1 =
  difference_ $ \_ _ _ ->
    Nothing


differenceWith0
  :: (a -> b -> Maybe a) -> RadixTree a -> RadixTree b -> RadixTree a
differenceWith0 f (RadixTree mA tA) (RadixTree mB tB) =
  let mC | Just xA <- mA, Just xB <- mB = f xA xB
         | otherwise                    = mA

  in RadixTree mC $ differenceWith1 f tA tB

differenceWith1
  :: (a -> b -> Maybe a) -> Radix1Tree a -> Radix1Tree b -> Radix1Tree a
differenceWith1 f =
  difference_ $ \s xA xB ->
    case s of
      L -> f xA xB
      R -> f xB xA

{-# INLINE difference_ #-}
difference_
  :: (forall x y. S x y a b -> x -> y -> Maybe a)
  -> Radix1Tree a
  -> Radix1Tree b
  -> Radix1Tree a
difference_ (f :: forall n o. S n o x y -> n -> o -> Maybe x) = anyAny L
  where
    anyAny :: forall a b. S a b x y -> Radix1Tree a -> Radix1Tree b -> Radix1Tree x
    anyAny s tA tB =
      case tA of
        Bin pA lA rA    -> binAny s (# pA, lA, rA #) tA tB

        Tip arrA mA dA  -> let !wA = indexByteArray arrA 0
                           in tipAny s (# wA, 0, arrA, mA, dA #) tA tB

        Nil             -> case s of
                             L -> Nil
                             R -> tB

    tipAny
      :: forall a b. S a b x y -> UTip a -> Radix1Tree a -> Radix1Tree b -> Radix1Tree x
    tipAny s uA@(# _, nA, arrA, mA, dA #) tA tB =
      case tB of
        Bin pB lB rB    -> tipBin s uA tA (# pB, lB, rB #) tB

        Tip arrB mB dB  -> let !wB = indexByteArray arrB 0

                               uB = (# wB, 0, arrB, mB, dB #)

                               !lenA = sizeofByteArray arrA - nA

                               !lenB = sizeofByteArray arrB

                           in if lenB > lenA
                                then tipTip s uA tA uB tB lenA

                                else let !(# s' #) = other s
                                     in tipTip s' uB tB uA tA lenB

        Nil             -> case s of
                             L | nA == 0   -> tA
                               | otherwise -> Tip (dropByteArray nA arrA) mA dA

                             R -> Nil

    tipTip
      :: forall a b. S a b x y
      -> UTip a -> Radix1Tree a -> UTip b -> Radix1Tree b -> Int -> Radix1Tree x
    tipTip s (# wA0, nA, arrA, mA, dA #) tA (# wB0, nB, arrB, mB, dB #) tB len =
      go wA0 wB0 1
      where
        go wA wB !o
          | wA == wB  =
              let !nB' = nB + o
                  !wB' = indexByteArray arrB nB'

              in if o >= len
                   then let !arrA' | nA == 0   = arrA
                                   | otherwise = dropByteArray nA arrA

                        in if nB' == sizeofByteArray arrB
                             then let mC | Just xA <- mA, Just xB <- mB = f s xA xB
                                         | otherwise =
                                             case s of
                                               L -> mA
                                               R -> mB

                                  in retip arrA' mC (anyAny s dA dB)

                             else let mA' = case s of
                                              L -> mA
                                              R -> Nothing

                                  in retip arrA' mA' $
                                       let !(# s' #) = other s
                                       in tipAny s' (# wB', nB', arrB, mB, dB #) tB dA

                   else let !nA' = nA + o
                            !wA' = indexByteArray arrA nA'

                        in go wA' wB' (o + 1)

          | otherwise =
              case s of
                L | nA == 0   -> tA
                  | otherwise -> Tip (dropByteArray nA arrA) mA dA

                R | nB == 0   -> tB
                  | otherwise -> Tip (dropByteArray nB arrB) mB dB

    binAny
      :: forall a b. S a b x y -> UBin a -> Radix1Tree a -> Radix1Tree b -> Radix1Tree x
    binAny s uA tA tB =
      case tB of
        Bin pB lB rB    -> binBin s uA tA (# pB, lB, rB #) tB

        Tip arrB mB dB  -> let !(# s' #) = other s

                               !wB = indexByteArray arrB 0

                           in tipBin s' (# wB, 0, arrB, mB, dB #) tB uA tA

        Nil             -> case s of
                             L -> tA
                             R -> tB

    tipBin
      :: forall a b. S a b x y
      -> UTip a -> Radix1Tree a -> UBin b -> Radix1Tree b -> Radix1Tree x
    tipBin s uA@(# wA, nA, arrA, mA, dA #) tA (# pB, lB, rB #) tB
      | beyond pB wA = case s of
                         L | nA == 0   -> tA
                           | otherwise -> Tip (dropByteArray nA arrA) mA dA

                         R -> tB

      | wA < pB      = case s of
                         L -> tipAny s uA tA lB
                         R -> rebinL pB (tipAny s uA tA lB) rB

      | otherwise    = case s of
                         L -> tipAny s uA tA rB
                         R -> rebinR pB lB (tipAny s uA tA rB)

    binBin
      :: forall a b. S a b x y
      -> UBin a -> Radix1Tree a -> UBin b -> Radix1Tree b -> Radix1Tree x
    binBin s uA@(# pA, lA, rA #) tA uB@(# pB, lB, rB #) tB =
      let no = case s of
                 L -> tA
                 R -> tB

      in case Prelude.compare pA pB of
           EQ                  -> rebin pA (anyAny s lA lB) (anyAny s rA rB)

           LT | pB <= upper pA -> case s of
                                    L -> rebinR pA lA (binAny R uB tB rA)
                                    R -> binAny L uB tB rA

              | pA >= lower pB -> case s of
                                    L -> binAny s uA tA lB
                                    R -> rebinL pB (binAny s uA tA lB) rB

              | otherwise      -> no

           GT | pA <= upper pB -> case s of
                                    L -> binAny s uA tA rB
                                    R -> rebinR pB lB (binAny s uA tA rB)

              | pB >= lower pA -> case s of
                                    L -> rebinL pA (binAny R uB tB lA) rA
                                    R -> binAny L uB tB lA

              | otherwise      -> no



differenceWithKey0
  :: (Build -> a -> b -> Maybe a) -> RadixTree a -> RadixTree b -> RadixTree a
differenceWithKey0 f (RadixTree mA tA) (RadixTree mB tB) =
  let mC | Just xA <- mA, Just xB <- mB = f (Build Lin) xA xB
         | otherwise                    = mA

  in RadixTree mC $ differenceWithKey_
                      ( \s b arr xA xB ->
                           let b0 = Build $ Snoc b arr
                           in case s of
                                L -> f b0 xA xB
                                R -> f b0 xB xA
                      )
                      tA tB

differenceWithKey1
  :: (Build1 -> a -> b -> Maybe a) -> Radix1Tree a -> Radix1Tree b -> Radix1Tree a
differenceWithKey1 f =
  differenceWithKey_ $ \s b arr xA xB ->
    let b1 = Build1 $ b :/ arr
    in case s of
         L -> f b1 xA xB
         R -> f b1 xB xA

{-# INLINE differenceWithKey_ #-}
differenceWithKey_
  :: (forall x y. S x y a b -> Tsil ByteArray -> ByteArray -> x -> y -> Maybe a)
  -> Radix1Tree a
  -> Radix1Tree b
  -> Radix1Tree a
differenceWithKey_
  (f :: forall n o. S n o x y -> Tsil ByteArray -> ByteArray -> n -> o -> Maybe x) =
    anyAny L Lin
  where
    anyAny
      :: forall a b. S a b x y -> Tsil ByteArray
      -> Radix1Tree a -> Radix1Tree b -> Radix1Tree x
    anyAny s b tA tB =
      case tA of
        Bin pA lA rA    -> binAny s b (# pA, lA, rA #) tA tB

        Tip arrA mA dA  -> let !wA = indexByteArray arrA 0
                           in tipAny s b (# wA, 0, arrA, mA, dA #) tA tB

        Nil             -> case s of
                             L -> Nil
                             R -> tB

    tipAny
      :: forall a b. S a b x y -> Tsil ByteArray
      -> UTip a -> Radix1Tree a -> Radix1Tree b -> Radix1Tree x
    tipAny s b uA@(# _, nA, arrA, mA, dA #) tA tB =
      case tB of
        Bin pB lB rB    -> tipBin s b uA tA (# pB, lB, rB #) tB

        Tip arrB mB dB  -> let !wB = indexByteArray arrB 0

                               uB = (# wB, 0, arrB, mB, dB #)

                               !lenA = sizeofByteArray arrA - nA

                               !lenB = sizeofByteArray arrB

                           in if lenB > lenA
                                then tipTip s b uA tA uB tB lenA

                                else let !(# s' #) = other s
                                     in tipTip s' b uB tB uA tA lenB

        Nil             -> case s of
                             L | nA == 0   -> tA
                               | otherwise -> Tip (dropByteArray nA arrA) mA dA

                             R -> Nil

    tipTip
      :: forall a b. S a b x y -> Tsil ByteArray
      -> UTip a -> Radix1Tree a -> UTip b -> Radix1Tree b -> Int -> Radix1Tree x
    tipTip s b (# wA0, nA, arrA, mA, dA #) tA (# wB0, nB, arrB, mB, dB #) tB len =
      go wA0 wB0 1
      where
        go wA wB !o
          | wA == wB  =
              let !nB' = nB + o
                  !wB' = indexByteArray arrB nB'

              in if o >= len
                   then let !arrA' | nA == 0   = arrA
                                   | otherwise = dropByteArray nA arrA

                        in if nB' == sizeofByteArray arrB
                             then let mC | Just xA <- mA, Just xB <- mB =
                                             f s b arrA' xA xB

                                         | otherwise =
                                             case s of
                                               L -> mA
                                               R -> mB

                                  in retip arrA' mC (anyAny s (Snoc b arrA') dA dB)

                             else let mA' = case s of
                                              L -> mA
                                              R -> Nothing

                                  in retip arrA' mA' $
                                       let !(# s' #) = other s
                                       in tipAny s' (Snoc b arrA')
                                            (# wB', nB', arrB, mB, dB #) tB dA

                   else let !nA' = nA + o
                            !wA' = indexByteArray arrA nA'

                        in go wA' wB' (o + 1)

          | otherwise =
              case s of
                L | nA == 0   -> tA
                  | otherwise -> Tip (dropByteArray nA arrA) mA dA

                R | nB == 0   -> tB
                  | otherwise -> Tip (dropByteArray nB arrB) mB dB

    binAny
      :: forall a b. S a b x y -> Tsil ByteArray
      -> UBin a -> Radix1Tree a -> Radix1Tree b -> Radix1Tree x
    binAny s b uA tA tB =
      case tB of
        Bin pB lB rB    -> binBin s b uA tA (# pB, lB, rB #) tB

        Tip arrB mB dB  -> let !(# s' #) = other s

                               !wB = indexByteArray arrB 0

                           in tipBin s' b (# wB, 0, arrB, mB, dB #) tB uA tA

        Nil             -> case s of
                             L -> tA
                             R -> tB

    tipBin
      :: forall a b. S a b x y -> Tsil ByteArray
      -> UTip a -> Radix1Tree a -> UBin b -> Radix1Tree b -> Radix1Tree x
    tipBin s b uA@(# wA, nA, arrA, mA, dA #) tA (# pB, lB, rB #) tB
      | beyond pB wA = case s of
                         L | nA == 0   -> tA
                           | otherwise -> Tip (dropByteArray nA arrA) mA dA

                         R -> tB

      | wA < pB      = case s of
                         L -> tipAny s b uA tA lB
                         R -> rebinL pB (tipAny s b uA tA lB) rB

      | otherwise    = case s of
                         L -> tipAny s b uA tA rB
                         R -> rebinR pB lB (tipAny s b uA tA rB)

    binBin
      :: forall a b. S a b x y -> Tsil ByteArray
      -> UBin a -> Radix1Tree a -> UBin b -> Radix1Tree b -> Radix1Tree x
    binBin s b uA@(# pA, lA, rA #) tA uB@(# pB, lB, rB #) tB =
      let no = case s of
                 L -> tA
                 R -> tB

      in case Prelude.compare pA pB of
           EQ                  -> rebin pA (anyAny s b lA lB) (anyAny s b rA rB)

           LT | pB <= upper pA -> case s of
                                    L -> rebinR pA lA (binAny R b uB tB rA)
                                    R -> binAny L b uB tB rA

              | pA >= lower pB -> case s of
                                    L -> binAny s b uA tA lB
                                    R -> rebinL pB (binAny s b uA tA lB) rB

              | otherwise      -> no

           GT | pA <= upper pB -> case s of
                                    L -> binAny s b uA tA rB
                                    R -> rebinR pB lB (binAny s b uA tA rB)

              | pB >= lower pA -> case s of
                                    L -> rebinL pA (binAny R b uB tB lA) rA
                                    R -> binAny L b uB tB lA

              | otherwise      -> no



compare0 :: (a -> b -> Bool) -> RadixTree a -> RadixTree b -> PartialOrdering
compare0 f (RadixTree mA tA) (RadixTree mB tB) =
  let o = case mA of
            Just xA -> case mB of
                         Just xB
                           | f xA xB   -> Equal
                           | otherwise -> Incomparable

                         Nothing -> Superset

            Nothing -> case mB of
                         Just _  -> Subset
                         Nothing -> Equal

  in order o $ Data.RadixNTree.Word8.Strict.compare1 f tA tB

compare1 :: (a -> b -> Bool) -> Radix1Tree a -> Radix1Tree b -> PartialOrdering
compare1 (f :: x -> y -> Bool) = anyAny L
  where
    anyAny :: forall a b. S a b x y -> Radix1Tree a -> Radix1Tree b -> PartialOrdering
    anyAny s tA tB =
      case tA of
        Bin pA lA rA    -> binAny s (# pA, lA, rA #) tA tB

        Tip arrA mA dA  -> let !wA = indexByteArray arrA 0
                           in tipAny s (# wA, 0, arrA, mA, dA #) tA tB

        Nil             -> case tB of
                             Nil -> Equal
                             _   -> case s of
                                      L -> Subset
                                      R -> Superset

    tipAny
      :: forall a b. S a b x y -> UTip a -> Radix1Tree a -> Radix1Tree b -> PartialOrdering
    tipAny s uA@(# _, nA, arrA, _, _ #) tA tB =
      case tB of
        Bin pB lB rB    -> tipBin s uA tA (# pB, lB, rB #)

        Tip arrB mB dB  -> let !wB = indexByteArray arrB 0

                               uB = (# wB, 0, arrB, mB, dB #)

                               !lenA = sizeofByteArray arrA - nA

                               !lenB = sizeofByteArray arrB

                           in if lenB > lenA
                                then tipTip s uA uB tB lenA

                                else let !(# s' #) = other s
                                     in tipTip s' uB uA tA lenB

        Nil             -> case s of
                             L -> Superset
                             R -> Subset

    tipTip
      :: forall a b. S a b x y -> UTip a -> UTip b -> Radix1Tree b -> Int -> PartialOrdering
    tipTip s (# wA0, nA, arrA, mA, dA #) (# wB0, nB, arrB, mB, dB #) tB len =
      go wA0 wB0 1
      where
        go wA wB !o
          | wA == wB  =
              let !nB' = nB + o
                  !wB' = indexByteArray arrB nB'

              in if o >= len
                   then if nB' == sizeofByteArray arrB
                          then let o_ = case mA of
                                          Just xA -> case mB of
                                                       Just xB ->
                                                         let eq = case s of
                                                                    L -> f xA xB
                                                                    R -> f xB xA

                                                         in if eq
                                                              then Equal
                                                              else Incomparable

                                                       Nothing -> case s of
                                                                    L -> Superset
                                                                    R -> Subset
                                          Nothing -> case mB of
                                                       Just _  -> case s of
                                                                    L -> Subset
                                                                    R -> Superset

                                                       Nothing -> Equal

                               in order o_ $ anyAny s dA dB

                          else let !(# s' #) = other s
                               in tipAny s' (# wB', nB', arrB, mB, dB #) tB dA

                   else let !nA' = nA + o
                            !wA' = indexByteArray arrA nA'

                        in go wA' wB' (o + 1)

          | otherwise = case s of
                             L -> Superset
                             R -> Subset

    binAny
      :: forall a b. S a b x y -> UBin a -> Radix1Tree a -> Radix1Tree b -> PartialOrdering
    binAny s uA tA tB =
      case tB of
        Bin pB lB rB    -> binBin s uA tA (# pB, lB, rB #) tB

        Tip arrB mB dB  -> let !(# s' #) = other s

                               !wB = indexByteArray arrB 0

                           in tipBin s' (# wB, 0, arrB, mB, dB #) tB uA

        Nil             -> case s of
                             L -> Superset
                             R -> Subset

    tipBin :: forall a b. S a b x y -> UTip a -> Radix1Tree a -> UBin b -> PartialOrdering
    tipBin s uA@(# wA, _, _, _, _ #) tA (# pB, lB, rB #)
      | beyond pB wA = Incomparable
      | otherwise    = limit s . tipAny s uA tA $ if wA < pB
                                                     then lB
                                                     else rB

    binBin
      :: forall a b. S a b x y
      -> UBin a -> Radix1Tree a -> UBin b -> Radix1Tree b -> PartialOrdering
    binBin s uA@(# pA, lA, rA #) tA uB@(# pB, lB, rB #) tB =
      case Prelude.compare pA pB of
        EQ                  -> order (anyAny s lA lB) (anyAny s rA rB)

        LT | pB <= upper pA -> let !(# s' #) = other s
                               in limit s' $ binAny s' uB tB rA
           | pA >= lower pB -> limit s $ binAny s uA tA lB
           | otherwise      -> Incomparable

        GT | pA <= upper pB -> limit s $ binAny s uA tA rB
           | pB >= lower pA -> let !(# s' #) = other s
                               in limit s' $ binAny s' uB tB lA
           | otherwise      -> Incomparable



disjoint0 :: RadixTree a -> RadixTree b -> Bool
disjoint0 (RadixTree mA tA) (RadixTree mB tB) =
  let mC | Just _ <- mA, Just _ <- mB = False
         | otherwise                  = True

  in mC && disjoint1 tA tB

disjoint1 :: Radix1Tree a -> Radix1Tree b -> Bool
disjoint1 = anyAny
  where
    anyAny :: forall a b. Radix1Tree a -> Radix1Tree b -> Bool
    anyAny tA tB =
      case tA of
        Bin pA lA rA    -> binAny (# pA, lA, rA #) tA tB

        Tip arrA mA dA  -> let !wA = indexByteArray arrA 0
                           in tipAny (# wA, 0, arrA, mA, dA #) tA tB

        Nil             -> True

    tipAny :: forall a b. UTip a -> Radix1Tree a -> Radix1Tree b -> Bool
    tipAny uA@(# _, nA, arrA, _, _ #) tA tB =
      case tB of
        Bin pB lB rB    -> tipBin uA tA (# pB, lB, rB #)

        Tip arrB mB dB  -> let !wB = indexByteArray arrB 0

                               uB = (# wB, 0, arrB, mB, dB #)

                               !lenA = sizeofByteArray arrA - nA

                               !lenB = sizeofByteArray arrB

                           in if lenB > lenA
                                then tipTip uA uB tB lenA

                                else tipTip uB uA tA lenB

        Nil             -> True

    tipTip :: forall a b. UTip a -> UTip b -> Radix1Tree b -> Int -> Bool
    tipTip (# wA0, nA, arrA, mA, dA #) (# wB0, nB, arrB, mB, dB #) tB len = go wA0 wB0 1
      where
        go wA wB !o
          | wA == wB  =
              let !nB' = nB + o
                  !wB' = indexByteArray arrB nB'

              in if o >= len
                   then if nB' == sizeofByteArray arrB
                          then let mC | Just _ <- mA, Just _ <- mB = False
                                      | otherwise                  = True

                               in mC && anyAny dA dB

                          else tipAny (# wB', nB', arrB, mB, dB #) tB dA

                   else let !nA' = nA + o
                            !wA' = indexByteArray arrA nA'

                        in go wA' wB' (o + 1)

          | otherwise = True

    binAny :: forall a b. UBin a -> Radix1Tree a -> Radix1Tree b -> Bool
    binAny uA tA tB =
      case tB of
        Bin pB lB rB    -> binBin uA tA (# pB, lB, rB #) tB

        Tip arrB mB dB  -> let !wB = indexByteArray arrB 0

                           in tipBin (# wB, 0, arrB, mB, dB #) tB uA

        Nil             -> True

    tipBin :: forall a b. UTip a -> Radix1Tree a -> UBin b -> Bool
    tipBin uA@(# wA, _, _, _, _ #) tA (# pB, lB, rB #)
      | beyond pB wA = True
      | otherwise    = tipAny uA tA $ if wA < pB
                                        then lB
                                        else rB

    binBin :: forall a b. UBin a -> Radix1Tree a -> UBin b -> Radix1Tree b -> Bool
    binBin uA@(# pA, lA, rA #) tA uB@(# pB, lB, rB #) tB =
      case Prelude.compare pA pB of
        EQ                  -> anyAny lA lB && anyAny rA rB

        LT | pB <= upper pA -> binAny uB tB rA
           | pA >= lower pB -> binAny uA tA lB
           | otherwise      -> True

        GT | pA <= upper pB -> binAny uA tA rB
           | pB >= lower pA -> binAny uB tB lA
           | otherwise      -> True



intersection0 :: RadixTree a -> RadixTree a -> RadixTree a
intersection0 (RadixTree mA tA) (RadixTree mB tB) =
  let mC | Just _ <- mA, Just _ <- mB = mA
         | otherwise                  = Nothing

  in RadixTree mC (intersection1 tA tB)

intersection1 :: Radix1Tree a -> Radix1Tree a -> Radix1Tree a
intersection1 = anyAny
  where
    anyAny tA tB =
      case tA of
        Bin pA lA rA    -> binAny (# pA, lA, rA #) tA tB

        Tip arrA mA dA  -> let !wA = indexByteArray arrA 0
                           in tipAny (# wA, 0, arrA, mA, dA #) tA tB

        Nil             -> Nil

    tipAny uA@(# _, nA, arrA, _, _ #) tA tB =
      case tB of
        Bin pB lB rB    -> tipBin uA tA (# pB, lB, rB #)

        Tip arrB mB dB  -> let !wB = indexByteArray arrB 0

                               uB = (# wB, 0, arrB, mB, dB #)

                               !lenA = sizeofByteArray arrA - nA

                               !lenB = sizeofByteArray arrB

                           in if lenB > lenA
                                then tipTip uA uB tB lenA

                                else tipTip uB uA tA lenB

        Nil             -> Nil

    tipTip (# wA0, nA, arrA, mA, dA #) (# wB0, nB, arrB, mB, dB #) tB len = go wA0 wB0 1
      where
        go wA wB !o
          | wA == wB  =
              let !nB' = nB + o
                  !wB' = indexByteArray arrB nB'

              in if o >= len
                   then let !arrA' | nA == 0   = arrA
                                   | otherwise = dropByteArray nA arrA

                        in if nB' == sizeofByteArray arrB
                             then let mC | Just _ <- mA, Just _ <- mB = mA
                                         | otherwise                  = Nothing

                                  in retip arrA' mC (anyAny dA dB)

                             else retip arrA' Nothing $
                                    tipAny (# wB', nB', arrB, mB, dB #) tB dA

                   else let !nA' = nA + o
                            !wA' = indexByteArray arrA nA'

                        in go wA' wB' (o + 1)

          | otherwise = Nil

    binAny uA tA tB =
      case tB of
        Bin pB lB rB    -> binBin uA tA (# pB, lB, rB #) tB

        Tip arrB mB dB  -> let !wB = indexByteArray arrB 0

                           in tipBin (# wB, 0, arrB, mB, dB #) tB uA

        Nil             -> Nil

    tipBin uA@(# wA, _, _, _, _ #) tA (# pB, lB, rB #)
      | beyond pB wA = Nil
      | otherwise    = tipAny uA tA $ if wA < pB
                                        then lB
                                        else rB

    binBin uA@(# pA, lA, rA #) tA uB@(# pB, lB, rB #) tB =
      case Prelude.compare pA pB of
        EQ                  -> rebin pA (anyAny lA lB) (anyAny rA rB)

        LT | pB <= upper pA -> binAny uB tB rA
           | pA >= lower pB -> binAny uA tA lB
           | otherwise      -> Nil

        GT | pA <= upper pB -> binAny uA tA rB
           | pB >= lower pA -> binAny uB tB lA
           | otherwise      -> Nil



intersectionL0 :: RadixTree a -> RadixTree b -> RadixTree a
intersectionL0 (RadixTree mA tA) (RadixTree mB tB) =
  let mC | Just _ <- mA, Just _ <- mB = mA
         | otherwise                  = Nothing

  in RadixTree mC (intersectionL1 tA tB)

intersectionL1 :: Radix1Tree a -> Radix1Tree b -> Radix1Tree a
intersectionL1 =
  intersection_ $ \s a b ->
    let !(# c #) = case s of
                     L -> (# a #)
                     R -> (# b #)
    in Just c


intersectionWith0' :: (a -> b -> c) -> RadixTree a -> RadixTree b -> RadixTree c
intersectionWith0' f (RadixTree mA tA) (RadixTree mB tB) =
  let mC | Just a <- mA, Just b <- mB = Just $! f a b
         | otherwise                  = Nothing

  in RadixTree mC (intersectionWith1' f tA tB)

intersectionWith1' :: (a -> b -> c) -> Radix1Tree a -> Radix1Tree b -> Radix1Tree c
intersectionWith1' f =
  intersection_ $ \s a b ->
    Just $! case s of
              L -> f a b
              R -> f b a

{-# INLINE intersection_ #-}
intersection_
  :: (forall x y. S x y a b -> x -> y -> Maybe c)
  -> Radix1Tree a
  -> Radix1Tree b
  -> Radix1Tree c
intersection_ (f :: forall n o. S n o x y -> n -> o -> Maybe c) = anyAny L
  where
    anyAny :: forall a b. S a b x y -> Radix1Tree a -> Radix1Tree b -> Radix1Tree c
    anyAny s tA tB =
      case tA of
        Bin pA lA rA    -> binAny s (# pA, lA, rA #) tA tB

        Tip arrA mA dA  -> let !wA = indexByteArray arrA 0
                           in tipAny s (# wA, 0, arrA, mA, dA #) tA tB

        Nil             -> Nil

    tipAny
      :: forall a b. S a b x y -> UTip a -> Radix1Tree a -> Radix1Tree b -> Radix1Tree c
    tipAny s uA@(# _, nA, arrA, _, _ #) tA tB =
      case tB of
        Bin pB lB rB    -> tipBin s uA tA (# pB, lB, rB #)

        Tip arrB mB dB  -> let !wB = indexByteArray arrB 0

                               uB = (# wB, 0, arrB, mB, dB #)

                               !lenA = sizeofByteArray arrA - nA

                               !lenB = sizeofByteArray arrB

                           in if lenB > lenA
                                then tipTip s uA uB tB lenA

                                else let !(# s' #) = other s
                                     in tipTip s' uB uA tA lenB

        Nil             -> Nil

    tipTip
      :: forall a b. S a b x y -> UTip a -> UTip b -> Radix1Tree b -> Int -> Radix1Tree c
    tipTip s (# wA0, nA, arrA, mA, dA #) (# wB0, nB, arrB, mB, dB #) tB len =
      go wA0 wB0 1
      where
        go wA wB !o
          | wA == wB  =
              let !nB' = nB + o
                  !wB' = indexByteArray arrB nB'

              in if o >= len
                   then let !arrA' | nA == 0   = arrA
                                   | otherwise = dropByteArray nA arrA

                        in if nB' == sizeofByteArray arrB
                             then let mC | Just xA <- mA, Just xB <- mB = f s xA xB
                                         | otherwise                    = Nothing

                                  in retip arrA' mC (anyAny s dA dB)

                             else retip arrA' Nothing $
                                    let !(# s' #) = other s
                                    in tipAny s' (# wB', nB', arrB, mB, dB #) tB dA

                   else let !nA' = nA + o
                            !wA' = indexByteArray arrA nA'

                        in go wA' wB' (o + 1)

          | otherwise = Nil

    binAny
      :: forall a b. S a b x y -> UBin a -> Radix1Tree a -> Radix1Tree b -> Radix1Tree c
    binAny s uA tA tB =
      case tB of
        Bin pB lB rB    -> binBin s uA tA (# pB, lB, rB #) tB

        Tip arrB mB dB  -> let !(# s' #) = other s

                               !wB = indexByteArray arrB 0

                           in tipBin s' (# wB, 0, arrB, mB, dB #) tB uA

        Nil             -> Nil

    tipBin :: forall a b. S a b x y -> UTip a -> Radix1Tree a -> UBin b -> Radix1Tree c
    tipBin s uA@(# wA, _, _, _, _ #) tA (# pB, lB, rB #)
      | beyond pB wA = Nil
      | otherwise    = tipAny s uA tA $ if wA < pB
                                          then lB
                                          else rB

    binBin
      :: forall a b. S a b x y
      -> UBin a -> Radix1Tree a -> UBin b -> Radix1Tree b -> Radix1Tree c
    binBin s uA@(# pA, lA, rA #) tA uB@(# pB, lB, rB #) tB =
      case Prelude.compare pA pB of
        EQ                  -> rebin pA (anyAny s lA lB) (anyAny s rA rB)

        LT | pB <= upper pA -> let !(# s' #) = other s
                               in binAny s' uB tB rA
           | pA >= lower pB -> binAny s uA tA lB
           | otherwise      -> Nil

        GT | pA <= upper pB -> binAny s uA tA rB
           | pB >= lower pA -> let !(# s' #) = other s
                               in binAny s' uB tB lA
           | otherwise      -> Nil



intersectionWithKey0'
  :: (Build -> a -> b -> c) -> RadixTree a -> RadixTree b -> RadixTree c
intersectionWithKey0' f (RadixTree mA tA) (RadixTree mB tB) =
  let mC | Just a <- mA, Just b <- mB = Just $! f (Build Lin) a b
         | otherwise                  = Nothing

  in RadixTree mC $ intersectionWithKey_
                      ( \s b arr vA vB ->
                           Just $! let b0 = Build $ Snoc b arr
                                   in case s of
                                        L -> f b0 vA vB
                                        R -> f b0 vB vA
                      )
                      tA tB

intersectionWithKey1'
  :: (Build1 -> a -> b -> c) -> Radix1Tree a -> Radix1Tree b -> Radix1Tree c
intersectionWithKey1' f =
  intersectionWithKey_ $ \s b arr vA vB ->
    Just $! let b1 = Build1 $ b :/ arr
            in case s of
                 L -> f b1 vA vB
                 R -> f b1 vB vA

{-# INLINE intersectionWithKey_ #-}
intersectionWithKey_
  :: (forall x y. S x y a b -> Tsil ByteArray -> ByteArray -> x -> y -> Maybe c)
  -> Radix1Tree a
  -> Radix1Tree b
  -> Radix1Tree c
intersectionWithKey_
  (f :: forall n o. S n o x y -> Tsil ByteArray -> ByteArray -> n -> o -> Maybe c) =
    anyAny L Lin
  where
    anyAny
      :: forall a b. S a b x y -> Tsil ByteArray
      -> Radix1Tree a -> Radix1Tree b -> Radix1Tree c
    anyAny s b tA tB =
      case tA of
        Bin pA lA rA    -> binAny s b (# pA, lA, rA #) tA tB

        Tip arrA mA dA  -> let !wA = indexByteArray arrA 0
                           in tipAny s b (# wA, 0, arrA, mA, dA #) tA tB

        Nil             -> Nil

    tipAny
      :: forall a b. S a b x y -> Tsil ByteArray
      -> UTip a -> Radix1Tree a -> Radix1Tree b -> Radix1Tree c
    tipAny s b uA@(# _, nA, arrA, _, _ #) tA tB =
      case tB of
        Bin pB lB rB    -> tipBin s b uA tA (# pB, lB, rB #)

        Tip arrB mB dB  -> let !wB = indexByteArray arrB 0

                               uB = (# wB, 0, arrB, mB, dB #)

                               !lenA = sizeofByteArray arrA - nA

                               !lenB = sizeofByteArray arrB

                           in if lenB > lenA
                                then tipTip s b uA uB tB lenA

                                else let !(# s' #) = other s
                                     in tipTip s' b uB uA tA lenB

        Nil             -> Nil

    tipTip
      :: forall a b. S a b x y -> Tsil ByteArray
      -> UTip a -> UTip b -> Radix1Tree b -> Int -> Radix1Tree c
    tipTip s b (# wA0, nA, arrA, mA, dA #) (# wB0, nB, arrB, mB, dB #) tB len =
      go wA0 wB0 1
      where
        go wA wB !o
          | wA == wB  =
              let !nB' = nB + o
                  !wB' = indexByteArray arrB nB'

              in if o >= len
                   then let !arrA' | nA == 0   = arrA
                                   | otherwise = dropByteArray nA arrA

                        in if nB' == sizeofByteArray arrB
                             then let mC | Just xA <- mA, Just xB <- mB =
                                             f s b arrA' xA xB

                                         | otherwise                    = Nothing


                                  in retip arrA' mC (anyAny s (Snoc b arrA') dA dB)

                             else retip arrA' Nothing $
                                    let !(# s' #) = other s
                                    in tipAny s' (Snoc b arrA')
                                         (# wB', nB', arrB, mB, dB #) tB dA

                   else let !nA' = nA + o
                            !wA' = indexByteArray arrA nA'

                        in go wA' wB' (o + 1)

          | otherwise = Nil

    binAny
      :: forall a b. S a b x y -> Tsil ByteArray
      -> UBin a -> Radix1Tree a -> Radix1Tree b -> Radix1Tree c
    binAny s b uA tA tB =
      case tB of
        Bin pB lB rB    -> binBin s b uA tA (# pB, lB, rB #) tB

        Tip arrB mB dB  -> let !(# s' #) = other s

                               !wB = indexByteArray arrB 0

                           in tipBin s' b (# wB, 0, arrB, mB, dB #) tB uA

        Nil             -> Nil

    tipBin
      :: forall a b. S a b x y -> Tsil ByteArray
      -> UTip a -> Radix1Tree a -> UBin b -> Radix1Tree c
    tipBin s b uA@(# wA, _, _, _, _ #) tA (# pB, lB, rB #)
      | beyond pB wA = Nil
      | otherwise    = tipAny s b uA tA $ if wA < pB
                                            then lB
                                            else rB

    binBin
      :: forall a b. S a b x y -> Tsil ByteArray
      -> UBin a -> Radix1Tree a -> UBin b -> Radix1Tree b -> Radix1Tree c
    binBin s b uA@(# pA, lA, rA #) tA uB@(# pB, lB, rB #) tB =
      case Prelude.compare pA pB of
        EQ                  -> rebin pA (anyAny s b lA lB) (anyAny s b rA rB)

        LT | pB <= upper pA -> let !(# s' #) = other s
                               in binAny s' b uB tB rA
           | pA >= lower pB -> binAny s b uA tA lB
           | otherwise      -> Nil

        GT | pA <= upper pB -> binAny s b uA tA rB
           | pB >= lower pA -> let !(# s' #) = other s
                               in binAny s' b uB tB lA
           | otherwise      -> Nil



{-# INLINE merge0 #-}
merge0
  :: (Build -> a -> b -> Maybe c)
  -> (Build -> a -> Maybe c)
  -> (Build -> Radix1Tree a -> Radix1Tree c)
  -> (Build -> b -> Maybe c)
  -> (Build -> Radix1Tree b -> Radix1Tree c)
  -> RadixTree a
  -> RadixTree b
  -> RadixTree c
merge0 f oneX treeX oneY treeY = \(RadixTree mA tA) (RadixTree mB tB) ->
  let mC = case mA of
             Just xA -> case mB of
                          Just xB -> f (Build Lin) xA xB
                          Nothing -> oneX (Build Lin) xA

             Nothing -> case mB of
                          Just xB -> oneY (Build Lin) xB
                          Nothing -> Nothing

  in RadixTree mC $
       merge_ (\b arr -> f (Build $ Snoc b arr))
         (\b arr -> oneX (Build $ Snoc b arr)) treeX
         (\b arr -> oneY (Build $ Snoc b arr)) treeY
         tA tB

{-# INLINE merge1 #-}
merge1
  :: (Build1 -> a -> b -> Maybe c)
  -> (Build1 -> a -> Maybe c)
  -> (Build -> Radix1Tree a -> Radix1Tree c)
  -> (Build1 -> b -> Maybe c)
  -> (Build -> Radix1Tree b -> Radix1Tree c)
  -> Radix1Tree a
  -> Radix1Tree b
  -> Radix1Tree c
merge1 f oneX treeX oneY treeY =
  merge_ (\b arr -> f (Build1 $ b :/ arr))
    (\b arr -> oneX (Build1 $ b :/ arr)) treeX
    (\b arr -> oneY (Build1 $ b :/ arr)) treeY

{-# INLINE merge_ #-}
merge_
  :: (Tsil ByteArray -> ByteArray -> a -> b -> Maybe c)
  -> (Tsil ByteArray -> ByteArray -> a -> Maybe c)
  -> (Build -> Radix1Tree a -> Radix1Tree c)
  -> (Tsil ByteArray -> ByteArray -> b -> Maybe c)
  -> (Build -> Radix1Tree b -> Radix1Tree c)
  -> Radix1Tree a
  -> Radix1Tree b
  -> Radix1Tree c
merge_ (f :: Tsil ByteArray -> ByteArray -> x -> y -> Maybe c) oneX treeX oneY treeY =
  anyAny L Lin
  where
    sideA :: forall a b. S a b x y -> Tsil ByteArray -> Radix1Tree a -> Radix1Tree c
    sideA s b tA = case s of
                     L -> treeX (Build b) tA
                     R -> treeY (Build b) tA

    sideB :: forall a b. S a b x y -> Tsil ByteArray -> Radix1Tree b -> Radix1Tree c
    sideB s b tB = case s of
                     L -> treeY (Build b) tB
                     R -> treeX (Build b) tB

    anyAny
      :: forall a b. S a b x y -> Tsil ByteArray
      -> Radix1Tree a -> Radix1Tree b -> Radix1Tree c
    anyAny s b tA tB =
      case tA of
        Bin pA lA rA    -> binAny s b (# pA, lA, rA #) tA tB

        Tip arrA mA dA  -> let !wA = indexByteArray arrA 0
                           in tipAny s b (# wA, 0, arrA, mA, dA #) tA tB

        Nil             -> sideB s b tB

    tipAny
      :: forall a b. S a b x y -> Tsil ByteArray
      -> UTip a -> Radix1Tree a -> Radix1Tree b -> Radix1Tree c
    tipAny s b uA@(# _, nA, arrA, mA, dA #) tA tB =
      case tB of
        Bin pB lB rB    -> tipBin s b uA tA (# pB, lB, rB #) tB

        Tip arrB mB dB  -> let !wB = indexByteArray arrB 0

                               uB = (# wB, 0, arrB, mB, dB #)

                               !lenA = sizeofByteArray arrA - nA

                               !lenB = sizeofByteArray arrB

                           in if lenB > lenA
                                then tipTip s b uA tA uB tB lenA

                                else let !(# s' #) = other s
                                     in tipTip s' b uB tB uA tA lenB

        Nil             -> sideA s b $ if nA == 0
                                         then tA
                                         else Tip (dropByteArray nA arrA) mA dA

    tipTip
      :: forall a b. S a b x y -> Tsil ByteArray
      -> UTip a -> Radix1Tree a -> UTip b -> Radix1Tree b -> Int -> Radix1Tree c
    tipTip s b (# wA0, nA, arrA, mA, dA #) tA (# wB0, nB, arrB, mB, dB #) tB len =
      go wA0 wB0 1
      where
        go wA wB !o
          | wA == wB  =
              let !nB' = nB + o
                  !wB' = indexByteArray arrB nB'

              in if o >= len
                   then let !arrA' | nA == 0   = arrA
                                   | otherwise = dropByteArray nA arrA

                        in if nB' == sizeofByteArray arrB
                             then let mC = case mA of
                                             Just xA ->
                                               case mB of
                                                 Just xB -> case s of
                                                              L -> f b arrA' xA xB
                                                              R -> f b arrA' xB xA

                                                 Nothing -> case s of
                                                              L -> oneX b arrA' xA
                                                              R -> oneY b arrA' xA

                                             Nothing ->
                                               case mB of
                                                 Just xB -> case s of
                                                              L -> oneY b arrA' xB
                                                              R -> oneX b arrA' xB

                                                 Nothing -> Nothing

                                  in retip arrA' mC (anyAny s (Snoc b arrA') dA dB)

                             else let mC = case mA of
                                             Just xA -> case s of
                                                          L -> oneX b arrA' xA
                                                          R -> oneY b arrA' xA

                                             Nothing -> Nothing

                                  in retip arrA' mC $
                                       let !(# s' #) = other s
                                       in tipAny s' (Snoc b arrA')
                                            (# wB', nB', arrB, mB, dB #) tB dA

                   else let !nA' = nA + o
                            !wA' = indexByteArray arrA nA'

                        in go wA' wB' (o + 1)

          | o == 1 =
              safeJoin wA ( sideA s b $ if nA == 0
                                          then tA
                                          else Tip (dropByteArray nA arrA) mA dA
                          )
                       wB ( sideB s b $ if nB == 0
                                          then tB
                                          else Tip (dropByteArray nB arrB) mB dB
                          )

          | otherwise =
              let !o' = o - 1

                  !(# !arrC, !arrA' #) = splitByteArray nA o' arrA

                  !arrB' = dropByteArray (nB + o') arrB

              in retip arrC Nothing $ safeJoin wA (sideA s b $ Tip arrA' mA dA)
                                               wB (sideB s b $ Tip arrB' mB dB)

    binAny
      :: forall a b. S a b x y -> Tsil ByteArray
      -> UBin a -> Radix1Tree a -> Radix1Tree b -> Radix1Tree c
    binAny s b uA tA tB =
      case tB of
        Bin pB lB rB    -> binBin s b uA tA (# pB, lB, rB #) tB

        Tip arrB mB dB  -> let !(# s' #) = other s

                               !wB = indexByteArray arrB 0

                           in tipBin s' b (# wB, 0, arrB, mB, dB #) tB uA tA

        Nil             -> sideA s b tA

    tipBin
      :: forall a b. S a b x y -> Tsil ByteArray
      -> UTip a -> Radix1Tree a -> UBin b -> Radix1Tree b -> Radix1Tree c
    tipBin s b uA@(# wA, nA, arrA, mA, dA #) tA (# pB, lB, rB #) tB
      | beyond pB wA = safeJoin wA (sideA s b $ if nA == 0
                                                  then tA
                                                  else Tip (dropByteArray nA arrA) mA dA
                                   )
                                pB (sideB s b tB)

      | wA < pB      = rebin pB (tipAny s b uA tA lB) (sideB s b rB)

      | otherwise    = rebin pB (sideB s b lB) (tipAny s b uA tA rB)

    binBin
      :: forall a b. S a b x y -> Tsil ByteArray
      -> UBin a -> Radix1Tree a -> UBin b -> Radix1Tree b -> Radix1Tree c
    binBin s b uA@(# pA, lA, rA #) tA uB@(# pB, lB, rB #) tB =
      let {-# NOINLINE no #-}
          no = safeJoin pA (sideA s b tA) pB (sideB s b tB)

      in case Prelude.compare pA pB of
           EQ                  -> rebin pA (anyAny s b lA lB) (anyAny s b rA rB)

           LT | pB <= upper pA -> let !(# s' #) = other s

                                  in rebin pA (sideA s b lA) (binAny s' b uB tB rA)

              | pA >= lower pB -> rebin pB (binAny s b uA tA lB) (sideB s b rB)

              | otherwise      -> no

           GT | pA <= upper pB -> rebin pB (sideB s b lB) (binAny s b uA tA rB)

              | pB >= lower pA -> let !(# s' #) = other s

                                  in rebin pA (binAny s' b uB tB lA) (sideA s b rA)

              | otherwise      -> no



{-# INLINE insert0 #-}
insert0 :: Feed -> a -> RadixTree a -> RadixTree a
insert0 (Feed feed) a = \(RadixTree mx t) ->
  feed $ \step s ->
    case step s of
      More w z -> RadixTree mx $ insert_ a step w z t
      Done     -> RadixTree (Just a) t

{-# INLINE insert1 #-}
insert1 :: Feed1 -> a -> Radix1Tree a -> Radix1Tree a
insert1 (Feed1 w feed) a =
  feed $ \step -> insert_ a step w

{-# INLINE insert_ #-}
insert_ :: a -> (x -> Step Word8 x) -> Word8 -> x -> Radix1Tree a -> Radix1Tree a
insert_ a step = go
  where
    go !w !s t =
      case t of
        Bin p l r
          | beyond p w -> join
                            w (singleton_ step w s a)
                            p t

          | w < p      -> Bin p (go w s l) r
          | otherwise  -> Bin p l (go w s r)

        Tip arr mx dx -> goarr w s 0
          where
            goarr v !z n
              | v == indexByteArray arr n =
                  if n + 1 >= sizeofByteArray arr
                    then case step z of
                           More u z' -> Tip arr mx (go u z' dx)
                           Done      -> Tip arr (Just a) dx

                    else case step z of
                           More u z' -> goarr u z' (n + 1)
                           Done      ->
                             let !(# !brr, !crr #) = splitByteArray 0 (n + 1) arr
                             in Tip brr (Just a) (Tip crr mx dx)

              | n == 0    =
                  join
                    (indexByteArray arr 0) t
                    w (singleton_ step w s a)

              | otherwise =
                  let !(# !brr, !crr #) = splitByteArray 0 n arr
                  in Tip brr Nothing $
                       join
                         (indexByteArray crr 0) (Tip crr mx dx)
                         v (singleton_ step v z a)

        Nil -> singleton_ step w s a



{-# INLINE insertWith0 #-}
insertWith0 :: (a -> a) -> Feed -> a -> RadixTree a -> RadixTree a
insertWith0 f (Feed feed) a = \(RadixTree mx t) ->
  feed $ \step s ->
    case step s of
      More w z -> RadixTree mx $ insertWith_ f a step w z t
      Done     ->
        let y = case mx of
                  Just x  -> f x
                  Nothing -> a

        in RadixTree (Just y) t

{-# INLINE insertWith1 #-}
insertWith1 :: (a -> a) -> Feed1 -> a -> Radix1Tree a -> Radix1Tree a
insertWith1 f (Feed1 w feed) a =
  feed $ \step -> insertWith_ f a step w

{-# INLINE insertWith_ #-}
insertWith_
  :: (a -> a) -> a -> (x -> Step Word8 x) -> Word8 -> x -> Radix1Tree a -> Radix1Tree a
insertWith_ f a step = go
  where
    go !w !s t =
      case t of
        Bin p l r
          | beyond p w -> join
                            w (singleton_ step w s a)
                            p t

          | w < p      -> Bin p (go w s l) r
          | otherwise  -> Bin p l (go w s r)

        Tip arr mx dx -> goarr w s 0
          where
            goarr v !z n
              | v == indexByteArray arr n =
                  if n + 1 >= sizeofByteArray arr
                    then case step z of
                           More u z' -> Tip arr mx (go u z' dx)
                           Done      -> let y = case mx of
                                                  Just x  -> f x
                                                  Nothing -> a

                                        in Tip arr (Just y) dx

                    else case step z of
                           More u z' -> goarr u z' (n + 1)
                           Done      ->
                             let !(# !brr, !crr #) = splitByteArray 0 (n + 1) arr
                             in Tip brr (Just a) (Tip crr mx dx)

              | n == 0    =
                  join
                    (indexByteArray arr 0) t
                    w (singleton_ step w s a)

              | otherwise =
                  let !(# !brr, !crr #) = splitByteArray 0 n arr
                  in Tip brr Nothing $
                       join
                         (indexByteArray crr 0) (Tip crr mx dx)
                         v (singleton_ step v z a)

        Nil -> singleton_ step w s a



{-# INLINE insertWith0' #-}
insertWith0' :: (a -> a) -> Feed -> a -> RadixTree a -> RadixTree a
insertWith0' f (Feed feed) a = \(RadixTree mx t) ->
  feed $ \step s ->
    case step s of
      More w z -> RadixTree mx $ insertWith'_ f a step w z t
      Done     ->
        let !y = case mx of
                  Just x  -> f x
                  Nothing -> a

        in RadixTree (Just y) t

{-# INLINE insertWith1' #-}
insertWith1' :: (a -> a) -> Feed1 -> a -> Radix1Tree a -> Radix1Tree a
insertWith1' f (Feed1 w feed) a =
  feed $ \step -> insertWith'_ f a step w

{-# INLINE insertWith'_ #-}
insertWith'_
  :: (a -> a) -> a -> (x -> Step Word8 x) -> Word8 -> x -> Radix1Tree a -> Radix1Tree a
insertWith'_ f a step = go
  where
    go !w !s t =
      case t of
        Bin p l r
          | beyond p w -> join
                            w (singleton_ step w s $! a)
                            p t

          | w < p      -> Bin p (go w s l) r
          | otherwise  -> Bin p l (go w s r)

        Tip arr mx dx -> goarr w s 0
          where
            goarr v !z n
              | v == indexByteArray arr n =
                  if n + 1 >= sizeofByteArray arr
                    then case step z of
                           More u z' -> Tip arr mx (go u z' dx)
                           Done      -> let !y = case mx of
                                                   Just x  -> f x
                                                   Nothing -> a

                                        in Tip arr (Just y) dx

                    else case step z of
                           More u z' -> goarr u z' (n + 1)
                           Done      ->
                             let !(# !brr, !crr #) = splitByteArray 0 (n + 1) arr
                             in Tip brr (Just a) (Tip crr mx dx)

              | n == 0    =
                  join
                    (indexByteArray arr 0) t
                    w (singleton_ step w s a)

              | otherwise =
                  let !(# !brr, !crr #) = splitByteArray 0 n arr
                  in Tip brr Nothing $
                       join
                         (indexByteArray crr 0) (Tip crr mx dx)
                         v (singleton_ step v z a)

        Nil -> singleton_ step w s a



{-# INLINE adjust0 #-}
adjust0 :: (a -> a) -> Feed -> RadixTree a -> RadixTree a
adjust0 f (Feed feed) = \(RadixTree mx t) ->
  feed $ \step s ->
    case step s of
      More w z -> RadixTree mx $ adjust_ f step w z t
      Done     -> RadixTree (fmap f mx) t

{-# INLINE adjust1 #-}
adjust1 :: (a -> a) -> Feed1 -> Radix1Tree a -> Radix1Tree a
adjust1 f (Feed1 w feed) =
  feed $ \step -> adjust_ f step w

{-# INLINE adjust_ #-}
adjust_ :: (a -> a) -> (x -> Step Word8 x) -> Word8 -> x -> Radix1Tree a -> Radix1Tree a
adjust_ f step = go
  where
    go !w !s t =
      case t of
        Bin p l r
          | beyond p w -> t
          | w < p      -> Bin p (go w s l) r
          | otherwise  -> Bin p l (go w s r)

        Tip arr mx dx -> goarr w s 0
          where
            goarr v !z n
              | v == indexByteArray arr n =
                  if n + 1 >= sizeofByteArray arr
                    then case step z of
                           More u z' -> Tip arr mx (go u z' dx)
                           Done      -> Tip arr (fmap f mx) dx

                    else case step z of
                           More u z' -> goarr u z' (n + 1)
                           Done      -> t

              | otherwise = t

        Nil -> t



{-# INLINE adjust0' #-}
adjust0' :: (a -> a) -> Feed -> RadixTree a -> RadixTree a
adjust0' f (Feed feed) = \(RadixTree mx t) ->
  feed $ \step s ->
    case step s of
      More w z -> RadixTree mx $ adjust'_ f step w z t
      Done     -> RadixTree (fmap' f mx) t

{-# INLINE adjust1' #-}
adjust1' :: (a -> a) -> Feed1 -> Radix1Tree a -> Radix1Tree a
adjust1' f (Feed1 w feed) =
  feed $ \step -> adjust'_ f step w

{-# INLINE adjust'_ #-}
adjust'_ :: (a -> a) -> (x -> Step Word8 x) -> Word8 -> x -> Radix1Tree a -> Radix1Tree a
adjust'_ f step = go
  where
    go !w !s t =
      case t of
        Bin p l r
          | beyond p w -> t
          | w < p      -> Bin p (go w s l) r
          | otherwise  -> Bin p l (go w s r)

        Tip arr mx dx -> goarr w s 0
          where
            goarr v !z n
              | v == indexByteArray arr n =
                  if n + 1 >= sizeofByteArray arr
                    then case step z of
                           More u z' -> Tip arr mx (go u z' dx)
                           Done      -> Tip arr (fmap' f mx) dx

                    else case step z of
                           More u z' -> goarr u z' (n + 1)
                           Done      -> t

              | otherwise = t

        Nil -> t



{-# INLINE delete0 #-}
delete0 :: Feed -> RadixTree a -> RadixTree a
delete0 (Feed feed) = \(RadixTree mx t) ->
  feed $ \step s ->
    case step s of
      More w z -> RadixTree mx $ delete_ step w z t
      Done     -> RadixTree Nothing t

{-# INLINE delete1 #-}
delete1 :: Feed1 -> Radix1Tree a -> Radix1Tree a
delete1 (Feed1 w feed) =
  feed $ \step -> delete_ step w

{-# INLINE delete_ #-}
delete_ :: (x -> Step Word8 x) -> Word8 -> x -> Radix1Tree a -> Radix1Tree a
delete_ step = go
  where
    go !w !s t =
      case t of
        Bin p l r
          | beyond p w -> t
          | w < p      -> rebinL p (go w s l) r
          | otherwise  -> rebinR p l (go w s r)

        Tip arr mx dx -> goarr w s 0
          where
            goarr v !z n
              | v == indexByteArray arr n =
                  if n + 1 >= sizeofByteArray arr
                    then case step z of
                           More u z' -> retip arr mx (go u z' dx)
                           Done      -> retip arr Nothing dx

                    else case step z of
                           More u z' -> goarr u z' (n + 1)
                           Done      -> t

              | otherwise = t

        Nil          -> t



{-# INLINE prune0 #-}
prune0 :: Openness -> Feed -> RadixTree a -> RadixTree a
prune0 openness (Feed feed) = \(RadixTree mx t) ->
  feed $ \step s ->
    case step s of
      More w z -> RadixTree mx $ prune_ openness step w z t
      Done     ->
        let my = case openness of
                   Open   -> mx
                   Closed -> Nothing

        in RadixTree my Nil

{-# INLINE prune1 #-}
prune1 :: Openness -> Feed1 -> Radix1Tree a -> Radix1Tree a
prune1 openness (Feed1 w feed) =
  feed $ \step -> prune_ openness step w

{-# INLINE prune_ #-}
prune_ :: Openness -> (x -> Step Word8 x) -> Word8 -> x -> Radix1Tree a -> Radix1Tree a
prune_ openness step = go
  where
    go !w !s t =
      case t of
        Bin p l r
          | beyond p w -> t
          | w < p      -> rebinL p (go w s l) r
          | otherwise  -> rebinR p l (go w s r)

        Tip arr mx dx -> goarr w s 0
          where
            goarr v !z n
              | v == indexByteArray arr n =
                  if n + 1 >= sizeofByteArray arr
                    then case step z of
                           More u z' -> retip arr mx (go u z' dx)
                           Done      ->
                             case openness of
                               Open   -> retip arr mx Nil
                               Closed -> Nil

                    else case step z of
                           More u z' -> goarr u z' (n + 1)
                           Done      -> Nil

              | otherwise = t

        Nil          -> t



{-# INLINE update0 #-}
update0 :: (a -> Maybe a) -> Feed -> RadixTree a -> RadixTree a
update0 f (Feed feed) = \(RadixTree mx t) ->
  feed $ \step s ->
    case step s of
      More w z -> RadixTree mx $ update_ f step w z t
      Done     -> RadixTree (f =<< mx) t

{-# INLINE update1 #-}
update1 :: (a -> Maybe a) -> Feed1 -> Radix1Tree a -> Radix1Tree a
update1 f (Feed1 w feed) =
  feed $ \step -> update_ f step w

{-# INLINE update_ #-}
update_
  :: (a -> Maybe a) -> (x -> Step Word8 x) -> Word8 -> x -> Radix1Tree a -> Radix1Tree a
update_ f step = go
  where
    go !w !s t =
      case t of
        Bin p l r
          | beyond p w -> t
          | w < p      -> rebinL p (go w s l) r
          | otherwise  -> rebinR p l (go w s r)

        Tip arr mx dx -> goarr w s 0
          where
            goarr v !z n
              | v == indexByteArray arr n =
                  if n + 1 >= sizeofByteArray arr
                    then case step z of
                           More u z' -> retip arr mx (go u z' dx)
                           Done      -> retip arr (f =<< mx) dx

                    else case step z of
                           More u z' -> goarr u z' (n + 1)
                           Done      -> t

              | otherwise = t

        Nil         -> t



{-# INLINE alter0 #-}
alter0 :: (Maybe a -> Maybe a) -> Feed -> RadixTree a -> RadixTree a
alter0 f (Feed feed) = \(RadixTree mx t) ->
  feed $ \step s ->
    case step s of
      More w z -> RadixTree mx $ alter_ f step w z t
      Done     -> RadixTree (f mx) t

{-# INLINE alter1 #-}
alter1 :: (Maybe a -> Maybe a) -> Feed1 -> Radix1Tree a -> Radix1Tree a
alter1 f (Feed1 w feed) =
  feed $ \step -> alter_ f step w

{-# INLINE alter_ #-}
alter_
  :: (Maybe a -> Maybe a)
  -> (x -> Step Word8 x) -> Word8 -> x -> Radix1Tree a -> Radix1Tree a
alter_ f step = go
  where
    go !w !s t =
      case t of
        Bin p l r
          | beyond p w -> case f Nothing of
                            Nothing -> t
                            Just a  -> join
                                         w (singleton_ step w s a)
                                         p t

          | w < p      -> rebinL p (go w s l) r
          | otherwise  -> rebinR p l (go w s r)

        Tip arr mx dx -> goarr w s 0
          where
            goarr v !z n
              | v == indexByteArray arr n =
                  if n + 1 >= sizeofByteArray arr
                    then case step z of
                           More u z' -> retip arr mx (go u z' dx)
                           Done      -> retip arr (f mx) dx

                    else case step z of
                           More u z' -> goarr u z' (n + 1)
                           Done      ->
                             case f Nothing of
                               Nothing -> t
                               Just a  ->
                                 let !(# !brr, !crr #) = splitByteArray 0 (n + 1) arr
                                 in Tip brr (Just a) (Tip crr mx dx)

              | otherwise =
                  case f Nothing of
                    Nothing -> t
                    Just a  ->
                      if n == 0
                        then join
                               (indexByteArray arr 0) (Tip arr mx dx)
                               w (singleton_ step v z a)

                        else let !(# !brr, !crr #) = splitByteArray 0 n arr
                             in Tip brr Nothing $
                                  join
                                    (indexByteArray crr 0) (Tip crr mx dx)
                                    v (singleton_ step v z a)

        Nil       ->
          case f Nothing of
            Nothing -> t
            Just a  -> singleton_ step w s a



{-# INLINE shape0 #-}
shape0 :: (RadixTree a -> RadixTree a) -> Feed -> RadixTree a -> RadixTree a
shape0 f (Feed feed) = \t0@(RadixTree mx t) ->
  feed $ \step s ->
    case step s of
      More w z -> RadixTree mx $ shape_ f step w z t
      Done     -> f t0

{-# INLINE shape1 #-}
shape1 :: (RadixTree a -> RadixTree a) -> Feed1 -> Radix1Tree a -> Radix1Tree a
shape1 f (Feed1 w feed) =
  feed $ \step -> shape_ f step w

{-# INLINE shape_ #-}
shape_
  :: (RadixTree a -> RadixTree a)
  -> (x -> Step Word8 x) -> Word8 -> x -> Radix1Tree a -> Radix1Tree a
shape_ f step = go
  where
    go !w !s t =
      case t of
        Bin p l r
          | beyond p w -> let !(RadixTree my dy) = f (RadixTree Nothing Nil)
                          in case retip (fromStep step w s) my dy of
                               Nil -> t
                               dz  -> join
                                        w dz
                                        p t

          | w < p      -> rebinL p (go w s l) r
          | otherwise  -> rebinR p l (go w s r)

        Tip arr mx dx -> goarr w s 0
          where
            goarr v !z n
              | v == indexByteArray arr n =
                  let n' = n + 1
                  in if n' >= sizeofByteArray arr
                       then case step z of
                              More u z' -> retip arr mx (go u z' dx)
                              Done      -> let !(RadixTree my dy) = f (RadixTree mx dx)
                                           in retip arr my dy

                       else case step z of
                              More u z' -> goarr u z' n'
                              Done      ->
                                let !(# !brr, !crr #) = splitByteArray 0 n' arr

                                    !(RadixTree my dy) = f (RadixTree Nothing (Tip crr mx dx))

                                in retip brr my dy

              | otherwise =
                  let !(RadixTree my dy) = f (RadixTree Nothing Nil)
                  in case retip (fromStep step v z) my dy of
                       Nil -> t
                       dz  ->
                         if n == 0
                           then join
                                  (indexByteArray arr 0) (Tip arr mx dx)
                                  v dz

                           else let !(# !brr, !crr #) = splitByteArray 0 n arr
                                in Tip brr Nothing $
                                     join
                                       (indexByteArray crr 0) (Tip crr mx dx)
                                       v dz

        Nil       ->
          let !(RadixTree my dy) = f (RadixTree Nothing Nil)
          in retip (fromStep step w s) my dy



-- | Result of a tree split.
data Split l r = Split !(RadixTree l) !(RadixTree r)

{-# INLINE splitL0 #-}
splitL0 :: Openness -> Feed -> RadixTree a -> Split a a
splitL0 openness (Feed feed) = \(RadixTree mx t) ->
  feed $ \step s ->
    case step s of
      More w z ->
        let !(# !l, !r #) = splitL_ openness step w z t
        in Split (RadixTree mx l) (RadixTree Nothing r)

      Done     ->
        let !(# !my, !mz #) = case openness of
                                Open   -> (# Nothing, mx #)
                                Closed -> (# mx, Nothing #)

        in Split (RadixTree my Nil) (RadixTree mz t)

-- | Result of a tree split.
data Split1 l r = Split1 !(Radix1Tree l) !(Radix1Tree r)

{-# INLINE splitL1 #-}
splitL1 :: Openness -> Feed1 -> Radix1Tree a -> Split1 a a
splitL1 openness (Feed1 w feed) = \t ->
  feed $ \step s ->
    case splitL_ openness step w s t of
      (# !l, !r #) -> Split1 l r

{-# INLINE splitL_ #-}
splitL_
  :: Openness -> (x -> Step Word8 x)
  -> Word8 -> x -> Radix1Tree a -> (# Radix1Tree a, Radix1Tree a #)
splitL_ openness step = go
  where
    go !w !s t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then let !(# !ll, !lr #) = go w s l
                        in (# ll, rebinL p lr r #)

                   else (# Nil, t #)

            else if w <= upper p
                   then let !(# !rl, !rr #) = go w s r
                        in (# rebinR p l rl, rr #)

                   else (# t, Nil #)

        Tip arr mx dx -> goarr w s 0
          where
            goarr v !z n =
              let n' = n + 1
              in case indexByteArray arr n `compare` v of
                   EQ | n' >= sizeofByteArray arr ->
                          case step z of
                            More u z' ->
                              let !(# !dl, !dr #) = go u z' dx
                              in (# retip arr mx dl, retip arr Nothing dr #)

                            Done      ->
                              let !(# !my, !mz #) =
                                    case openness of
                                      Open   -> (# Nil             , mx      #)
                                      Closed -> (# retip arr mx Nil, Nothing #)

                              in (# my, retip arr mz dx #)

                      | otherwise ->
                          case step z of
                            More u z' -> goarr u z' n'
                            Done      -> (# Nil, t #)

                   LT -> (# t, Nil #)

                   GT -> (# Nil, t #)

        Nil -> (# Nil, Nil #)



-- | Result of a tree split with a lookup.
data SplitLookup l x r = SplitLookup !(RadixTree l) !(Maybe x) !(RadixTree r)

{-# INLINE splitLookup0 #-}
splitLookup0 :: Feed -> RadixTree a -> SplitLookup a a a
splitLookup0 (Feed feed) = \(RadixTree mx t) ->
  feed $ \step s ->
    case step s of
      More w z ->
        let !(# !l, !my, !r #) = splitLookup_ step w z t
        in SplitLookup (RadixTree mx l) my (RadixTree Nothing r)

      Done     -> SplitLookup (RadixTree Nothing Nil) mx (RadixTree Nothing t)

-- | Result of a tree split with a lookup.
data SplitLookup1 l x r = SplitLookup1 !(Radix1Tree l) !(Maybe x) !(Radix1Tree r)

{-# INLINE splitLookup1 #-}
splitLookup1 :: Feed1 -> Radix1Tree a -> SplitLookup1 a a a
splitLookup1 (Feed1 w feed) = \t ->
  feed $ \step s ->
    case splitLookup_ step w s t of
      (# !l, !mx, !r #) -> SplitLookup1 l mx r

{-# INLINE splitLookup_ #-}
splitLookup_
  :: (x -> Step Word8 x)
  -> Word8 -> x -> Radix1Tree a -> (# Radix1Tree a, Maybe a, Radix1Tree a #)
splitLookup_ step = go
  where
    go !w !s t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then let !(# !ll, !my, !lr #) = go w s l
                        in (# ll, my, rebinL p lr r #)

                   else (# Nil, Nothing, t #)

            else if w <= upper p
                   then let !(# !rl, !my, !rr #) = go w s r
                        in (# rebinR p l rl, my, rr #)

                   else (# t, Nothing, Nil #)

        Tip arr mx dx -> goarr w s 0
          where
            goarr v !z n =
              let n' = n + 1
              in case indexByteArray arr n `compare` v of
                   EQ | n' >= sizeofByteArray arr ->
                          case step z of
                            More u z' ->
                              let !(# !dl, !my, !dr #) = go u z' dx
                              in (# retip arr mx dl, my, retip arr Nothing dr #)

                            Done      ->
                              (# Nil, mx, retip arr Nothing dx #)

                      | otherwise ->
                          case step z of
                            More u z' -> goarr u z' n'
                            Done      -> (# Nil, Nothing, t #)

                   LT -> (# t, Nothing, Nil #)

                   GT -> (# Nil, Nothing, t #)

        Nil -> (# Nil, Nothing, Nil #)



{-# INLINE filterMaybe #-}
filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe f mx =
  case mx of
    Just x | f x -> Just x
    _            -> Nothing

filter0 :: (a -> Bool) -> RadixTree a -> RadixTree a
filter0 f (RadixTree mx t) = RadixTree (filterMaybe f mx) (filter1 f t)

filter1 :: (a -> Bool) -> Radix1Tree a -> Radix1Tree a
filter1 f = go
  where
    go t =
      case t of
        Bin p l r     -> rebin p (go l) (go r)
        Tip arr mx dx -> retip arr (filterMaybe f mx) (go dx)
        Nil           -> Nil



filterWithKey0 :: (Build -> a -> Bool) -> RadixTree a -> RadixTree a
filterWithKey0 f (RadixTree mx t) =
  RadixTree (filterMaybe (f (Build Lin)) mx) $
    filterWithKey_ (\b arr -> f (Build $ Snoc b arr)) t

filterWithKey1 :: (Build1 -> a -> Bool) -> Radix1Tree a -> Radix1Tree a
filterWithKey1 f = filterWithKey_ (\b arr -> f (Build1 $ b :/ arr))

{-# INLINE filterWithKey_ #-}
filterWithKey_
  :: (Tsil ByteArray -> ByteArray -> a -> Bool) -> Radix1Tree a -> Radix1Tree a
filterWithKey_ f = go Lin
  where
    go b t =
      case t of
        Bin p l r     -> rebin p (go b l) (go b r)

        Tip arr mx dx -> retip arr (filterMaybe (f b arr) mx) (go (Snoc b arr) dx)

        Nil           -> Nil



mapMaybe0 :: (a -> Maybe b) -> RadixTree a -> RadixTree b
mapMaybe0 f (RadixTree mx t) = RadixTree (f =<< mx) (mapMaybe1 f t)

mapMaybe1 :: (a -> Maybe b) -> Radix1Tree a -> Radix1Tree b
mapMaybe1 f = go
  where
    go t =
      case t of
        Bin p l r     -> rebin p (go l) (go r)
        Tip arr mx dx -> retip arr (f =<< mx) (go dx)
        Nil           -> Nil



mapMaybeWithKey0 :: (Build -> a -> Maybe b) -> RadixTree a -> RadixTree b
mapMaybeWithKey0 f (RadixTree mx t) =
  RadixTree (f (Build Lin) =<< mx) $
    mapMaybeWithKey_ (\b arr -> f (Build $ Snoc b arr)) Lin t

mapMaybeWithKey1 :: (Build1 -> a -> Maybe b) -> Radix1Tree a -> Radix1Tree b
mapMaybeWithKey1 f = mapMaybeWithKey_ (\b arr -> f (Build1 $ b :/ arr)) Lin

{-# INLINE mapMaybeWithKey_ #-}
mapMaybeWithKey_
  :: (Tsil ByteArray -> ByteArray -> a -> Maybe b) -> Tsil ByteArray
  -> Radix1Tree a -> Radix1Tree b
mapMaybeWithKey_ f = go
  where
    go b t =
      case t of
        Bin p l r     -> rebin p (go b l) (go b r)

        Tip arr mx dx -> retip arr (f b arr =<< mx) (go (Snoc b arr) dx)

        Nil           -> Nil



partition0 :: (a -> Bool) -> RadixTree a -> Split a a
partition0 f = \(RadixTree mx t) ->
  let !(# !l, !r #) = partition_ f t

      !(# !my, !mz #) =
        case mx of
          Just x
            | f x       -> (# mx     , Nothing #)
            | otherwise -> (# Nothing, mx      #)

          Nothing       -> (# Nothing, Nothing #)

  in Split (RadixTree my l) (RadixTree mz r)

partition1 :: (a -> Bool) -> Radix1Tree a -> Split1 a a
partition1 f = \t ->
  case partition_ f t of
    (# !l, !r #) -> Split1 l r

partition_ :: (a -> Bool) -> Radix1Tree a -> (# Radix1Tree a, Radix1Tree a #)
partition_ f = go
  where
    go t =
      case t of
        Bin p l r   ->
          let !(# !ly, !lz #) = go l
              !(# !ry, !rz #) = go r

          in (# rebin p ly ry, rebin p lz rz #)

        Tip arr mx dx ->
          let !(# !dy, !dz #) = go dx
          in case mx of
               Nothing -> (# retip arr Nothing dy, retip arr Nothing dz #)
               Just x  ->
                 if f x
                   then (# Tip   arr (Just x) dy, retip arr Nothing  dz #)
                   else (# retip arr Nothing  dy, Tip   arr (Just x) dz #)

        Nil         -> (# Nil, Nil #)



partitionWithKey0 :: (Build -> a -> Bool) -> RadixTree a -> Split a a
partitionWithKey0 f = \(RadixTree mx t) ->
  let !(# !l, !r #) = partitionWithKey_ (\b arr -> f (Build $ Snoc b arr)) t

      !(# !my, !mz #) =
        case mx of
          Just x
            | f (Build Lin) x -> (# mx     , Nothing #)
            | otherwise       -> (# Nothing, mx      #)

          Nothing             -> (# Nothing, Nothing #)

  in Split (RadixTree my l) (RadixTree mz r)

partitionWithKey1 :: (Build1 -> a -> Bool) -> Radix1Tree a -> Split1 a a
partitionWithKey1 f = \t ->
  case partitionWithKey_ (\b arr -> f (Build1 $ b :/ arr)) t of
    (# !l, !r #) -> Split1 l r

{-# INLINE partitionWithKey_ #-}
partitionWithKey_
  :: (Tsil ByteArray -> ByteArray -> a -> Bool)
  -> Radix1Tree a -> (# Radix1Tree a, Radix1Tree a #)
partitionWithKey_ f = go Lin
  where
    go b t =
      case t of
        Bin p l r     ->
          let !(# !ly, !lz #) = go b l
              !(# !ry, !rz #) = go b r

          in (# rebin p ly ry, rebin p lz rz #)

        Tip arr mx dx ->
          let !(# !dy, !dz #) = go (Snoc b arr) dx
          in case mx of
               Nothing -> (# retip arr Nothing dy, retip arr Nothing dz #)
               Just x  ->
                 if f b arr x
                   then (# Tip   arr (Just x) dy, retip arr Nothing  dz #)
                   else (# retip arr Nothing  dy, Tip   arr (Just x) dz #)

        Nil         -> (# Nil, Nil #)



mapEither0 :: (a -> Either b c) -> RadixTree a -> Split b c
mapEither0 f = \(RadixTree mx t) ->
  let !(# !l, !r #) = mapEither_ f t

      !(# !my, !mz #) =
        case mx of
          Just x ->
            case f x of
              Left y  -> (# Just y , Nothing #)
              Right z -> (# Nothing, Just z  #)

          Nothing     -> (# Nothing, Nothing #)

  in Split (RadixTree my l) (RadixTree mz r)

mapEither1 :: (a -> Either b c) -> Radix1Tree a -> Split1 b c
mapEither1 f = \t ->
  case mapEither_ f t of
    (# !l, !r #) -> Split1 l r

mapEither_ :: (a -> Either b c) -> Radix1Tree a -> (# Radix1Tree b, Radix1Tree c #)
mapEither_ f = go
  where
    go t =
      case t of
        Bin p l r     ->
          let !(# !ly, !lz #) = go l
              !(# !ry, !rz #) = go r

          in (# rebin p ly ry, rebin p lz rz #)

        Tip arr mx dx ->
          let !(# !dy, !dz #) = go dx
          in case mx of
               Nothing -> (# retip arr Nothing dy, retip arr Nothing dz #)
               Just x  ->
                 case f x of
                   Left y  -> (# Tip   arr (Just y) dy, retip arr Nothing  dz #)
                   Right z -> (# retip arr Nothing  dy, Tip   arr (Just z) dz #)

        Nil         -> (# Nil, Nil #)



mapEitherWithKey0 :: (Build -> a -> Either b c) -> RadixTree a -> Split b c
mapEitherWithKey0 f = \(RadixTree mx t) ->
  let !(# !l, !r #) = mapEitherWithKey_ (\b arr -> f (Build $ Snoc b arr)) t

      !(# !my, !mz #) =
        case mx of
          Just x ->
            case f (Build Lin) x of
              Left y  -> (# Just y , Nothing #)
              Right z -> (# Nothing, Just z  #)

          Nothing     -> (# Nothing, Nothing #)

  in Split (RadixTree my l) (RadixTree mz r)

mapEitherWithKey1 :: (Build1 -> a -> Either b c) -> Radix1Tree a -> Split1 b c
mapEitherWithKey1 f = \t ->
  case mapEitherWithKey_ (\b arr -> f (Build1 $ b :/ arr)) t of
    (# !l, !r #) -> Split1 l r

{-# INLINE mapEitherWithKey_ #-}
mapEitherWithKey_
  :: (Tsil ByteArray -> ByteArray -> a -> Either b c)
  -> Radix1Tree a -> (# Radix1Tree b, Radix1Tree c #)
mapEitherWithKey_ f = go Lin
  where
    go b t =
      case t of
        Bin p l r     ->
          let !(# !ly, !lz #) = go b l
              !(# !ry, !rz #) = go b r

          in (# rebin p ly ry, rebin p lz rz #)

        Tip arr mx dx ->
          let !(# !dy, !dz #) = go (Snoc b arr) dx
          in case mx of
               Nothing -> (# retip arr Nothing dy, retip arr Nothing dz #)
               Just x  ->
                 case f b arr x of
                   Left y  -> (# Tip   arr (Just y) dy, retip arr Nothing  dz #)
                   Right z -> (# retip arr Nothing  dy, Tip   arr (Just z) dz #)

        Nil         -> (# Nil, Nil #)



moduleLoc1 :: String
moduleLoc1 = "Radix1Tree.Word8.Strict"



lookupMin0 :: RadixTree a -> Maybe a
lookupMin0 (RadixTree mx t) =
  case mx of
    Just x  -> Just x
    Nothing -> lookupMin1 t

lookupMin1 :: Radix1Tree a -> Maybe a
lookupMin1 Nil = Nothing
lookupMin1 t   = let !(# a #) = unsafeLookupMin1 t
                 in Just a

unsafeLookupMin1 :: Radix1Tree a -> (# a #)
unsafeLookupMin1 t =
  case t of
    Bin _ l _   -> unsafeLookupMin1 l
    Tip _ mx dx -> case mx of
                     Just x  -> (# x #)
                     Nothing -> unsafeLookupMin1 dx

    Nil         -> throw $ MalformedTree moduleLoc1 "lookupMin"



lookupMinWithKey0 :: RadixTree a -> Maybe (Lookup a)
lookupMinWithKey0 (RadixTree mx t) =
  case mx of
    Just x  -> Just (Lookup (Build Lin) x)
    Nothing ->
      case t of
        Nil -> Nothing
        _   -> let !(# b, arr, a #) = unsafeLookupMinWithKey_ Lin t
               in Just $! Lookup (Build $ Snoc b arr) a

lookupMinWithKey1 :: Radix1Tree a -> Maybe (Lookup1 a)
lookupMinWithKey1 Nil = Nothing
lookupMinWithKey1 t   = Just $! unsafeLookupMinWithKey1 t

unsafeLookupMinWithKey1 :: Radix1Tree a -> Lookup1 a
unsafeLookupMinWithKey1 t =
  let !(# b, arr, a #) = unsafeLookupMinWithKey_ Lin t
  in Lookup1 (Build1 $ b :/ arr) a

unsafeLookupMinWithKey_
  :: Tsil ByteArray -> Radix1Tree a -> (# Tsil ByteArray, ByteArray, a #)
unsafeLookupMinWithKey_ = go
  where
    go b t =
      case t of
        Bin _ l _     -> go b l
        Tip arr mx dx -> case mx of
                           Just x  -> (# b, arr, x #)
                           Nothing -> go (Snoc b arr) dx

        Nil           -> throw $ MalformedTree moduleLoc1 "lookupMinWithKey"



lookupMax0 :: RadixTree a -> Maybe a
lookupMax0 (RadixTree mx t) =
  case t of
    Nil -> mx
    _   -> let !(# a #) = unsafeLookupMax1 t
           in Just a

lookupMax1 :: Radix1Tree a -> Maybe a
lookupMax1 Nil = Nothing
lookupMax1 t   = let !(# a #) = unsafeLookupMax1 t
                 in Just a

unsafeLookupMax1 :: Radix1Tree a -> (# a #)
unsafeLookupMax1 t =
  case t of
    Bin _ _ r   -> unsafeLookupMax1 r
    Tip _ mx dx -> case dx of
                     Nil | Just x <- mx -> (# x #)
                     _                  -> unsafeLookupMax1 dx

    Nil         -> throw $ MalformedTree moduleLoc1 "lookupMin"



lookupMaxWithKey0 :: RadixTree a -> Maybe (Lookup a)
lookupMaxWithKey0 (RadixTree mx t) =
  case t of
    Nil -> Lookup (Build Lin) `fmap'` mx
    _   -> let !(# b, arr, a #) = unsafeLookupMaxWithKey_ Lin t
           in Just $! Lookup (Build $ Snoc b arr) a

lookupMaxWithKey1 :: Radix1Tree a -> Maybe (Lookup1 a)
lookupMaxWithKey1 Nil = Nothing
lookupMaxWithKey1 t   = Just $! unsafeLookupMaxWithKey1 t

unsafeLookupMaxWithKey1 :: Radix1Tree a -> Lookup1 a
unsafeLookupMaxWithKey1 t =
  let !(# b, arr, a #) = unsafeLookupMaxWithKey_ Lin t
  in Lookup1 (Build1 $ b :/ arr) a

unsafeLookupMaxWithKey_
  :: Tsil ByteArray -> Radix1Tree a -> (# Tsil ByteArray, ByteArray, a #)
unsafeLookupMaxWithKey_ = go
  where
    go b t =
      case t of
        Bin _ _ r     -> go b r
        Tip arr mx dx -> case dx of
                           Nil | Just x <- mx -> (# b, arr, x #)
                           _                  -> go (Snoc b arr) dx

        Nil           -> throw $ MalformedTree moduleLoc1 "lookupMaxWithKey"



deleteMin0 :: RadixTree a -> RadixTree a
deleteMin0 (RadixTree mx t) =
  case mx of
    Just _  -> RadixTree Nothing t
    Nothing -> RadixTree mx (deleteMin1 t)

deleteMin1 :: Radix1Tree a -> Radix1Tree a
deleteMin1 Nil = Nil
deleteMin1 r   = unsafeDeleteMin1 r

unsafeDeleteMin1 :: Radix1Tree a -> Radix1Tree a
unsafeDeleteMin1 = go
  where
    go t =
      case t of
        Bin p l r     -> rebinL p (go l) r

        Tip arr mx dx -> case mx of
                           Nothing -> retip arr mx (go dx)
                           Just _  -> retip arr Nothing dx

        Nil           -> Nil



deleteMax0 :: RadixTree a -> RadixTree a
deleteMax0 t0@(RadixTree mx t) =
  case t of
    Nil -> case mx of
             Just _  -> RadixTree Nothing t
             Nothing -> t0

    _   -> RadixTree mx (unsafeDeleteMax1 t)

deleteMax1 :: Radix1Tree a -> Radix1Tree a
deleteMax1 Nil = Nil
deleteMax1 r   = unsafeDeleteMax1 r

unsafeDeleteMax1 :: Radix1Tree a -> Radix1Tree a
unsafeDeleteMax1 = go
  where
    go t =
      case t of
        Bin p l r     -> rebinR p l (go r)

        Tip arr mx dx -> case dx of
                           Nil     -> Nil
                           _       -> retip arr mx (go dx)

        Nil           -> Nil



adjustMin0 :: (a -> a) -> RadixTree a -> RadixTree a
adjustMin0 f (RadixTree mx t) =
  case mx of
    Just x  -> RadixTree (Just $ f x) t
    Nothing -> RadixTree mx (adjustMin1 f t)

adjustMin1 :: (a -> a) -> Radix1Tree a -> Radix1Tree a
adjustMin1 _ Nil = Nil
adjustMin1 f r   = unsafeAdjustMin1 f r

unsafeAdjustMin1 :: (a -> a) -> Radix1Tree a -> Radix1Tree a
unsafeAdjustMin1 f = go
  where
    go t =
      case t of
        Bin p l r     -> Bin p (go l) r

        Tip arr mx dx -> case mx of
                           Just x  -> Tip arr (Just $ f x) dx
                           Nothing -> Tip arr mx (go dx)

        Nil           -> Nil



adjustMin0' :: (a -> a) -> RadixTree a -> RadixTree a
adjustMin0' f (RadixTree mx t) =
  case mx of
    Just x  -> RadixTree (Just $! f x) t
    Nothing -> RadixTree mx (adjustMin1' f t)

adjustMin1' :: (a -> a) -> Radix1Tree a -> Radix1Tree a
adjustMin1' _ Nil = Nil
adjustMin1' f r   = unsafeAdjustMin1' f r

unsafeAdjustMin1' :: (a -> a) -> Radix1Tree a -> Radix1Tree a
unsafeAdjustMin1' f = go
  where
    go t =
      case t of
        Bin p l r     -> Bin p (go l) r

        Tip arr mx dx -> case mx of
                           Just x  -> Tip arr (Just $! f x) dx
                           Nothing -> Tip arr mx (go dx)

        Nil           -> Nil



adjustMinWithKey0 :: (Build -> a -> a) -> RadixTree a -> RadixTree a
adjustMinWithKey0 f (RadixTree mx t) =
  case mx of
    Just x  -> RadixTree (Just $ f (Build Lin) x) t
    Nothing -> RadixTree mx $
                 case t of
                   Nil -> Nil
                   _   -> unsafeAdjustMinWithKey_ (\b arr -> f (Build $ Snoc b arr)) t

adjustMinWithKey1 :: (Build1 -> a -> a) -> Radix1Tree a -> Radix1Tree a
adjustMinWithKey1 _ Nil = Nil
adjustMinWithKey1 f r   = unsafeAdjustMinWithKey1 f r

unsafeAdjustMinWithKey1 :: (Build1 -> a -> a) -> Radix1Tree a -> Radix1Tree a
unsafeAdjustMinWithKey1 f = unsafeAdjustMinWithKey_ (\b arr -> f (Build1 $ b :/ arr))

{-# INLINE unsafeAdjustMinWithKey_ #-}
unsafeAdjustMinWithKey_
  :: (Tsil ByteArray -> ByteArray -> a -> a) -> Radix1Tree a -> Radix1Tree a
unsafeAdjustMinWithKey_ f = go Lin
  where
    go b t =
      case t of
        Bin p l r     -> Bin p (go b l) r

        Tip arr mx dx -> case mx of
                           Just x  -> Tip arr (Just $ f b arr x) dx
                           Nothing -> Tip arr mx (go (Snoc b arr) dx)

        Nil           -> Nil



adjustMinWithKey0' :: (Build -> a -> a) -> RadixTree a -> RadixTree a
adjustMinWithKey0' f (RadixTree mx t) =
  case mx of
    Just x  -> RadixTree (Just $! f (Build Lin) x) t
    Nothing -> RadixTree mx $
                 case t of
                   Nil -> Nil
                   _   -> unsafeAdjustMinWithKey'_ (\b arr -> f (Build $ Snoc b arr)) t

adjustMinWithKey1' :: (Build1 -> a -> a) -> Radix1Tree a -> Radix1Tree a
adjustMinWithKey1' _ Nil = Nil
adjustMinWithKey1' f r   = unsafeAdjustMinWithKey1' f r

unsafeAdjustMinWithKey1' :: (Build1 -> a -> a) -> Radix1Tree a -> Radix1Tree a
unsafeAdjustMinWithKey1' f = unsafeAdjustMinWithKey'_ (\b arr -> f (Build1 $ b :/ arr))

{-# INLINE unsafeAdjustMinWithKey'_ #-}
unsafeAdjustMinWithKey'_
  :: (Tsil ByteArray -> ByteArray -> a -> a) -> Radix1Tree a -> Radix1Tree a
unsafeAdjustMinWithKey'_ f = go Lin
  where
    go b t =
      case t of
        Bin p l r     -> Bin p (go b l) r

        Tip arr mx dx -> case mx of
                           Just x  -> Tip arr (Just $! f b arr x) dx
                           Nothing -> Tip arr mx (go (Snoc b arr) dx)

        Nil           -> Nil



adjustMax0 :: (a -> a) -> RadixTree a -> RadixTree a
adjustMax0 f t0@(RadixTree mx t) =
  case t of
    Nil -> case mx of
             Just x  -> RadixTree (Just $ f x) t
             Nothing -> t0

    _   -> RadixTree mx (unsafeAdjustMax1 f t)

adjustMax1 :: (a -> a) -> Radix1Tree a -> Radix1Tree a
adjustMax1 _ Nil = Nil
adjustMax1 f r   = unsafeAdjustMax1 f r

unsafeAdjustMax1 :: (a -> a) -> Radix1Tree a -> Radix1Tree a
unsafeAdjustMax1 f = go
  where
    go t =
      case t of
        Bin p l r     -> Bin p l (go r)

        Tip arr mx dx -> case dx of
                           Nil | Just x <- mx -> Tip arr (Just $ f x) dx
                           _                  -> Tip arr mx (go dx)

        Nil           -> Nil



adjustMax0' :: (a -> a) -> RadixTree a -> RadixTree a
adjustMax0' f t0@(RadixTree mx t) =
  case t of
    Nil -> case mx of
             Just x  -> RadixTree (Just $! f x) t
             Nothing -> t0

    _   -> RadixTree mx (unsafeAdjustMax1 f t)

adjustMax1' :: (a -> a) -> Radix1Tree a -> Radix1Tree a
adjustMax1' _ Nil = Nil
adjustMax1' f r   = unsafeAdjustMax1' f r

unsafeAdjustMax1' :: (a -> a) -> Radix1Tree a -> Radix1Tree a
unsafeAdjustMax1' f = go
  where
    go t =
      case t of
        Bin p l r     -> Bin p l (go r)

        Tip arr mx dx -> case dx of
                           Nil | Just x <- mx -> Tip arr (Just $! f x) dx
                           _                  -> Tip arr mx (go dx)

        Nil           -> Nil



adjustMaxWithKey0 :: (Build -> a -> a) -> RadixTree a -> RadixTree a
adjustMaxWithKey0 f t0@(RadixTree mx t) =
  case t of
    Nil -> case mx of
             Just x  -> RadixTree (Just $ f (Build Lin) x) t
             Nothing -> t0

    _   -> RadixTree mx $
             unsafeAdjustMaxWithKey_ (\b arr -> f (Build $ Snoc b arr)) t

adjustMaxWithKey1 :: (Build1 -> a -> a) -> Radix1Tree a -> Radix1Tree a
adjustMaxWithKey1 _ Nil = Nil
adjustMaxWithKey1 f r   = unsafeAdjustMaxWithKey1 f r

unsafeAdjustMaxWithKey1 :: (Build1 -> a -> a) -> Radix1Tree a -> Radix1Tree a
unsafeAdjustMaxWithKey1 f = unsafeAdjustMaxWithKey_ (\b arr -> f (Build1 $ b :/ arr))

{-# INLINE unsafeAdjustMaxWithKey_ #-}
unsafeAdjustMaxWithKey_
  :: (Tsil ByteArray -> ByteArray -> a -> a) -> Radix1Tree a -> Radix1Tree a
unsafeAdjustMaxWithKey_ f = go Lin
  where
    go b t =
      case t of
        Bin p l r     -> Bin p l (go b r)

        Tip arr mx dx ->
          case dx of
            Nil | Just x <- mx -> Tip arr (Just $ f b arr x) dx
            _                  -> Tip arr mx (go (Snoc b arr) dx)

        Nil           -> Nil



adjustMaxWithKey0' :: (Build -> a -> a) -> RadixTree a -> RadixTree a
adjustMaxWithKey0' f t0@(RadixTree mx t) =
  case t of
    Nil -> case mx of
             Just x  -> RadixTree (Just $! f (Build Lin) x) t
             Nothing -> t0

    _   -> RadixTree mx $
             unsafeAdjustMaxWithKey'_ (\b arr -> f (Build $ Snoc b arr)) t

adjustMaxWithKey1' :: (Build1 -> a -> a) -> Radix1Tree a -> Radix1Tree a
adjustMaxWithKey1' _ Nil = Nil
adjustMaxWithKey1' f r   = unsafeAdjustMaxWithKey1' f r

unsafeAdjustMaxWithKey1' :: (Build1 -> a -> a) -> Radix1Tree a -> Radix1Tree a
unsafeAdjustMaxWithKey1' f = unsafeAdjustMaxWithKey'_ (\b arr -> f (Build1 $ b :/ arr))

{-# INLINE unsafeAdjustMaxWithKey'_ #-}
unsafeAdjustMaxWithKey'_
  :: (Tsil ByteArray -> ByteArray -> a -> a) -> Radix1Tree a -> Radix1Tree a
unsafeAdjustMaxWithKey'_ f = go Lin
  where
    go b t =
      case t of
        Bin p l r     -> Bin p l (go b r)

        Tip arr mx dx ->
          case dx of
            Nil | Just x <- mx -> Tip arr (Just $! f b arr x) dx
            _                  -> Tip arr mx (go (Snoc b arr) dx)

        Nil           -> Nil



updateMin0 :: (a -> Maybe a) -> RadixTree a -> RadixTree a
updateMin0 f (RadixTree mx t) =
  case mx of
    Just x  -> RadixTree (f x) t
    Nothing -> RadixTree mx (updateMin1 f t)

updateMin1 :: (a -> Maybe a) -> Radix1Tree a -> Radix1Tree a
updateMin1 _ Nil = Nil
updateMin1 f r   = unsafeUpdateMin1 f r

unsafeUpdateMin1 :: (a -> Maybe a) -> Radix1Tree a -> Radix1Tree a
unsafeUpdateMin1 f = go
  where
    go t =
      case t of
        Bin p l r     -> rebinL p (go l) r

        Tip arr mx dx -> case mx of
                           Just x  -> retip arr (f x) dx
                           Nothing -> retip arr mx (go dx)

        Nil           -> Nil



updateMinWithKey0 :: (Build -> a -> Maybe a) -> RadixTree a -> RadixTree a
updateMinWithKey0 f (RadixTree mx t) =
  case mx of
    Just x  -> RadixTree (f (Build Lin) x) t
    Nothing -> RadixTree mx $
                 case t of
                   Nil -> Nil
                   _   -> unsafeUpdateMinWithKey_ (\b arr -> f (Build $ Snoc b arr)) t

updateMinWithKey1 :: (Build1 -> a -> Maybe a) -> Radix1Tree a -> Radix1Tree a
updateMinWithKey1 _ Nil = Nil
updateMinWithKey1 f r   = unsafeUpdateMinWithKey1 f r

unsafeUpdateMinWithKey1 :: (Build1 -> a -> Maybe a) -> Radix1Tree a -> Radix1Tree a
unsafeUpdateMinWithKey1 f = unsafeUpdateMinWithKey_ (\b arr -> f (Build1 $ b :/ arr))

{-# INLINE unsafeUpdateMinWithKey_ #-}
unsafeUpdateMinWithKey_
  :: (Tsil ByteArray -> ByteArray -> a -> Maybe a) -> Radix1Tree a -> Radix1Tree a
unsafeUpdateMinWithKey_ f = go Lin
  where
    go b t =
      case t of
        Bin p l r     -> rebinL p (go b l) r

        Tip arr mx dx -> case mx of
                           Just x  -> retip arr (f b arr x) dx
                           Nothing -> retip arr mx (go (Snoc b arr) dx)

        Nil           -> Nil



updateMax0 :: (a -> Maybe a) -> RadixTree a -> RadixTree a
updateMax0 f t0@(RadixTree mx t) =
  case t of
    Nil -> case mx of
             Just x  -> RadixTree (f x) t
             Nothing -> t0

    _   -> RadixTree mx (unsafeUpdateMax1 f t)

updateMax1 :: (a -> Maybe a) -> Radix1Tree a -> Radix1Tree a
updateMax1 _ Nil = Nil
updateMax1 f r   = unsafeUpdateMax1 f r

unsafeUpdateMax1 :: (a -> Maybe a) -> Radix1Tree a -> Radix1Tree a
unsafeUpdateMax1 f = go
  where
    go t =
      case t of
        Bin p l r     -> rebinR p l (go r)

        Tip arr mx dx -> case dx of
                           Nil | Just x <- mx -> retip arr (f x) dx
                           _                  -> retip arr mx (go dx)

        Nil           -> Nil



updateMaxWithKey0 :: (Build -> a -> Maybe a) -> RadixTree a -> RadixTree a
updateMaxWithKey0 f t0@(RadixTree mx t) =
  case t of
    Nil -> case mx of
             Just x  -> RadixTree (f (Build Lin) x) t
             Nothing -> t0

    _   -> RadixTree mx $
             unsafeUpdateMaxWithKey_ (\b arr -> f (Build $ Snoc b arr)) t

updateMaxWithKey1 :: (Build1 -> a -> Maybe a) -> Radix1Tree a -> Radix1Tree a
updateMaxWithKey1 _ Nil = Nil
updateMaxWithKey1 f r   = unsafeUpdateMaxWithKey1 f r

unsafeUpdateMaxWithKey1 :: (Build1 -> a -> Maybe a) -> Radix1Tree a -> Radix1Tree a
unsafeUpdateMaxWithKey1 f = unsafeUpdateMaxWithKey_ (\b arr -> f (Build1 $ b :/ arr))

{-# INLINE unsafeUpdateMaxWithKey_ #-}
unsafeUpdateMaxWithKey_
  :: (Tsil ByteArray -> ByteArray -> a -> Maybe a) -> Radix1Tree a -> Radix1Tree a
unsafeUpdateMaxWithKey_ f = go Lin
  where
    go b t =
      case t of
        Bin p l r     -> rebinR p l (go b r)

        Tip arr mx dx -> case dx of
                           Nil | Just x <- mx -> retip arr (f b arr x) dx
                           _                  -> retip arr mx (go (Snoc b arr) dx)

        Nil           -> Nil



-- | The leftmost value with its key and the rest of the tree.
data ViewL a = ViewL !Build a !(RadixTree a)
               deriving Show

minView0 :: RadixTree a -> Maybe (ViewL a)
minView0 (RadixTree mx t) =
  case mx of
    Just x  -> Just $! ViewL (Build Lin) x (RadixTree Nothing t)
    Nothing ->
      case t of
        Nil -> Nothing
        _   -> Just $! let !(# !b, !arr, x, !t' #) = unsafeMinView_ t
                       in ViewL (Build $ Snoc b arr) x (RadixTree mx t')


-- | The leftmost value with its key and the rest of the tree.
data ViewL1 a = ViewL1 !Build1 a !(Radix1Tree a)
                deriving Show

minView1 :: Radix1Tree a -> Maybe (ViewL1 a)
minView1 Nil = Nothing
minView1 t   = Just $! unsafeMinView1 t

unsafeMinView1 :: Radix1Tree a -> ViewL1 a
unsafeMinView1 t =
  let !(# !b, !arr, x, !t' #) = unsafeMinView_ t
  in ViewL1 (Build1 $ b :/ arr) x t'

unsafeMinView_ :: Radix1Tree a -> (# Tsil ByteArray, ByteArray, a, Radix1Tree a #)
unsafeMinView_ = go Lin
  where
    go b t =
      case t of
        Bin p l r     ->
          let !(# !b', !brr, z, !l' #) = go b l
          in (# b', brr, z, rebinL p l' r #)

        Tip arr mx dx ->
          case mx of
            Just x  -> (# b, arr, x, retip arr Nothing dx #)
            Nothing ->
              let !(# !b', !brr, z, !dy #) = go (Snoc b arr) dx
              in (# b', brr, z, retip arr mx dy #)

        Nil           -> throw $ MalformedTree moduleLoc1 "minView"



-- | The rightmost value with its key and the rest of the tree.
data ViewR a = ViewR !(RadixTree a) !Build a
               deriving Show

maxView0 :: RadixTree a -> Maybe (ViewR a)
maxView0 (RadixTree mx t) =
  case t of
    Nil -> ViewR (RadixTree Nothing t) (Build Lin) `fmap'` mx
    _   -> Just $! let !(# !t', !b, !arr, x #) = unsafeMaxView_ t
                   in ViewR (RadixTree mx t') (Build $ Snoc b arr) x


-- | The rightmost value with its key and the rest of the tree.
data ViewR1 a = ViewR1 !(Radix1Tree a) !Build1 a
                deriving Show

maxView1 :: Radix1Tree a -> Maybe (ViewR1 a)
maxView1 Nil = Nothing
maxView1 t   = Just $! unsafeMaxView1 t

unsafeMaxView1 :: Radix1Tree a -> ViewR1 a
unsafeMaxView1 t =
  let !(# !t', !b, !arr, x #) = unsafeMaxView_ t
  in ViewR1 t' (Build1 $ b :/ arr) x

unsafeMaxView_ :: Radix1Tree a -> (# Radix1Tree a, Tsil ByteArray, ByteArray, a #)
unsafeMaxView_ = go Lin
  where
    go b t =
      case t of
        Bin p l r     ->
          let !(# !r', !b', !brr, z #) = go b r
          in (# rebinR p l r', b', brr, z #)

        Tip arr mx dx ->
          case dx of
            Nil | Just x <- mx -> (# retip arr Nothing dx, b, arr, x #)
            _                  ->
              let !(# !dy, !b', !brr, z #) = go (Snoc b arr) dx
              in (# retip arr mx dy, b', brr, z #)

        Nil           -> throw $ MalformedTree moduleLoc1 "maxView"
