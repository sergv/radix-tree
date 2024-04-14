{-# LANGUAGE BangPatterns
           , DeriveLift
           , GADTs
           , RankNTypes
           , ScopedTypeVariables
           , UnboxedTuples #-}

module Data.Patricia.Word.Strict.Internal
  ( StrictPatricia
  , Patricia (..)

  , Data.Patricia.Word.Strict.Internal.null
  , size

  , Data.Patricia.Word.Strict.Internal.map
  , map'
  , mapWithKey
  , mapWithKey'

  , Data.Patricia.Word.Strict.Internal.foldl
  , Data.Patricia.Word.Strict.Internal.foldl'
  , foldlWithKey
  , foldlWithKey'

  , Data.Patricia.Word.Strict.Internal.foldr
  , Data.Patricia.Word.Strict.Internal.foldr'
  , foldrWithKey
  , foldrWithKey'

  , Data.Patricia.Word.Strict.Internal.foldMap
  , foldMapWithKey

  , Data.Patricia.Word.Strict.Internal.traverse
  , traverseWithKey

  , union
  , unionL
  , unionWith'
  , unionWithKey'

  , difference
  , differenceWith
  , differenceWithKey

  , Data.Patricia.Word.Strict.Internal.compare

  , disjoint
  , intersection
  , intersectionL
  , intersectionWith'
  , intersectionWithKey'

  , merge

  , Data.Patricia.Word.Strict.Internal.lookup
  , Data.Patricia.Word.Strict.Internal.find
  , member
  , takeOne

  , dirtyLookup
  , dirtyFind
  , dirtyMember

  , insert
  , insertWith
  , insertWith'

  , adjust
  , adjust'

  , delete

  , update

  , alter

  , lookupL
  , lookupR

  , adjustL
  , adjustL'
  , adjustLWithKey
  , adjustLWithKey'

  , adjustR
  , adjustR'
  , adjustRWithKey
  , adjustRWithKey'

  , deleteL
  , deleteR

  , updateL
  , updateR
  , updateLWithKey
  , updateRWithKey

  , adjustRange
  , unsafeAdjustRange

  , adjustRange'
  , unsafeAdjustRange'

  , adjustRangeWithKey
  , unsafeAdjustRangeWithKey

  , adjustRangeWithKey'
  , unsafeAdjustRangeWithKey'

  , deleteRange
  , unsafeDeleteRange

  , updateRange
  , unsafeUpdateRange

  , updateRangeWithKey
  , unsafeUpdateRangeWithKey

  , takeRange
  , unsafeTakeRange

  , takeL
  , takeR

  , Split (..)
  , splitL
  , splitR

  , SplitLookup (..)
  , splitLookup

  , Data.Patricia.Word.Strict.Internal.filter
  , filterWithKey

  , mapMaybe
  , mapMaybeWithKey

  , partition
  , partitionWithKey

  , mapEither
  , mapEitherWithKey

  , lookupMin
  , lookupMinWithKey
  , lookupMax
  , lookupMaxWithKey

  , unsafeLookupMin
  , unsafeLookupMinWithKey
  , unsafeLookupMax
  , unsafeLookupMaxWithKey

  , deleteMin
  , deleteMax

  , adjustMin
  , adjustMin'
  , adjustMinWithKey
  , adjustMinWithKey'
  , adjustMax
  , adjustMax'
  , adjustMaxWithKey
  , adjustMaxWithKey'

  , updateMin
  , updateMinWithKey
  , updateMax
  , updateMaxWithKey

  , ViewL (..)
  , minView
  , unsafeMinView

  , ViewR (..)
  , maxView
  , unsafeMaxView
  ) where

import           Data.Patricia.Word.Common
import           Radix.Common
import           Radix.Exception
import           Radix.Word.Common
import           Radix.Word.Foundation

import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception (throw)
import           Data.Bits
import           Data.Foldable
import           Data.Functor.Classes
import           Language.Haskell.TH.Syntax (Lift)
import           Text.Read
import           Text.Show



-- | Convenience synonym.
type StrictPatricia = Patricia

-- | Spine-strict PATRICIA tree.
data Patricia a = Bin
                    {-# UNPACK #-} !Prefix
                    !(Patricia a)          -- ^ Masked bit is @0@.
                    !(Patricia a)          -- ^ Masked bit is @1@.

                | Tip
                    {-# UNPACK #-} !Key
                    a

                | Nil -- ^ Invariant: only allowed as the root of the tree.
                  deriving Lift

instance Show a => Show (Patricia a) where
  showsPrec = liftShowsPrec showsPrec showList

instance Show1 Patricia where
  liftShowsPrec showsPrec_ showList_ _ t =
    showListWith (liftShowsPrec showsPrec_ showList_ 0) $
      foldrWithKey (\k a -> (:) (k, a)) [] t

instance Read a => Read (Patricia a) where
  readPrec = liftReadPrec readPrec readListPrec

instance Read1 Patricia where
  liftReadPrec readPrec_ readList_ =
    fmap (Data.Foldable.foldl' (\z (k, a) -> insert k a z) Nil)
      (liftReadListPrec readPrec_ readList_)


instance Eq a => Eq (Patricia a) where
  (==) = liftEq (==)

instance Eq1 Patricia where
  liftEq eq = go
    where
      go l r =
        case l of
          Bin p xl xr ->
            case r of
              Bin q yl yr -> p == q && go xl yl && go xr yr
              _           -> False

          Tip kA a ->
            case r of
              Tip kB b -> kA == kB && eq a b
              _        -> False

          Nil ->
            case r of
              Nil -> True
              _   -> False


-- | Uses 'Data.Patricia.Word.Strict.map'.
instance Functor Patricia where
  fmap = Data.Patricia.Word.Strict.Internal.map

instance Foldable Patricia where
  foldl = Data.Patricia.Word.Strict.Internal.foldl
  foldr = Data.Patricia.Word.Strict.Internal.foldr
  foldMap = Data.Patricia.Word.Strict.Internal.foldMap

  foldl' = Data.Patricia.Word.Strict.Internal.foldl'
  foldr' = Data.Patricia.Word.Strict.Internal.foldr'

  null = Data.Patricia.Word.Strict.Internal.null
  length = fromIntegral . size

instance Traversable Patricia where
  traverse = Data.Patricia.Word.Strict.Internal.traverse


instance NFData a => NFData (Patricia a) where
  rnf = liftRnf rnf

instance NFData1 Patricia where
  liftRnf nf = go
    where
      go t =
        case t of
          Bin _ l r -> go l `seq` go r
          Tip _ a   -> nf a
          Nil       -> ()



{-# INLINE join #-}
-- | Knowing that the prefices of two non-'Nil' trees disagree, construct a 'Bin'.
join :: Prefix -> Patricia a -> Prefix -> Patricia a -> Patricia a
join p0 t0 p1 t1 =
  let m = branchingBit p0 p1

      p = mask p0 m .|. m

  in if zeroBit p0 m
       then Bin p t0 t1
       else Bin p t1 t0

{-# INLINE safeJoin #-}
-- | Knowing that the prefices of two trees disagree, construct a 'Bin'.
safeJoin :: Prefix -> Patricia a -> Prefix -> Patricia a -> Patricia a
safeJoin _  Nil _  t1  = t1
safeJoin _  t0  _  Nil = t0
safeJoin p0 t0  p1 t1  = join p0 t0 p1 t1

{-# INLINE rebin #-}
-- | Reconstruct a 'Bin' knowing that either of the sides may now be a 'Nil'.
rebin :: Prefix -> Patricia a -> Patricia a -> Patricia a
rebin p l r =
  case l of
    Nil -> r
    _   ->
      case r of
        Nil -> l
        _   -> Bin p l r

{-# INLINE rebinL #-}
-- | Reconstruct a 'Bin' knowing that the left side may now be a 'Nil'.
rebinL :: Prefix -> Patricia a -> Patricia a -> Patricia a
rebinL p l r =
  case l of
    Nil -> r
    _   -> Bin p l r


{-# INLINE rebinR #-}
-- | Reconstruct a 'Bin' knowing that the right side may now be a 'Nil'.
rebinR :: Prefix -> Patricia a -> Patricia a -> Patricia a
rebinR p l r =
  case r of
    Nil -> l
    _   -> Bin p l r


{-# INLINE retip #-}
-- | Reconstruct a 'Tip' knowing that the value may not be there anymore.
retip :: Key -> Maybe a -> Patricia a
retip w (Just a) = Tip w a
retip _ Nothing  = Nil



-- | \(\mathcal{O}(1)\).
--   Check if the tree is empty.
null :: Patricia a -> Bool
null Nil = True
null _   = False

-- | \(\mathcal{O}(n)\).
--   Calculate the number of elements stored in the tree.
--   The returned number is guaranteed to be non-negative.
size :: Patricia a -> Int
size t =
  case t of
    Bin _ l r -> let !m = size l
                     !n = size r
                 in m + n

    Tip _ _   -> 1

    Nil       -> 0



-- | \(\mathcal{O}(n)\).
--   Apply a function to every value in the tree.
map :: (a -> b) -> Patricia a -> Patricia b
map f = go
  where
    go t =
      case t of
        Bin p l r -> Bin p (go l) (go r)
        Tip k a   -> Tip k (f a)
        Nil       -> Nil

-- | \(\mathcal{O}(n)\).
--   Apply a function to every value in the tree.
--
--   New values are evaluated to WHNF.
map' :: (a -> b) -> Patricia a -> Patricia b
map' f = go
  where
    go t =
      case t of
        Bin p l r -> Bin p (go l) (go r)
        Tip k a   -> Tip k $! f a
        Nil       -> Nil

-- | \(\mathcal{O}(n)\).
--   Apply a function to every value in the tree.
mapWithKey :: (Word -> a -> b) -> Patricia a -> Patricia b
mapWithKey f = go
  where
    go t =
      case t of
        Bin p l r -> Bin p (go l) (go r)
        Tip k a   -> Tip k (f k a)
        Nil       -> Nil

-- | \(\mathcal{O}(n)\).
--   Apply a function to every value in the tree.
--
--   New values are evaluated to WHNF.
mapWithKey' :: (Word -> a -> b) -> Patricia a -> Patricia b
mapWithKey' f = go
  where
    go t =
      case t of
        Bin p l r -> Bin p (go l) (go r)
        Tip k a   -> Tip k $! f k a
        Nil       -> Nil



-- | \(\mathcal{O}(n_R)\).
--   Fold the tree left-to-right.
foldl :: (b -> a -> b) -> b -> Patricia a -> b
foldl f = go
  where
    go z t =
      case t of
        Bin _ l r -> go (go z l) r
        Tip _ a   -> f z a
        Nil       -> z

-- | \(\mathcal{O}(n_R)\).
--   Fold the tree left-to-right.
foldlWithKey :: (b -> Word -> a -> b) -> b -> Patricia a -> b
foldlWithKey f = go
  where
    go z t =
      case t of
        Bin _ l r -> go (go z l) r
        Tip k a   -> f z k a
        Nil       -> z



-- | \(\mathcal{O}(n)\).
--   Fold the tree left-to-right with a strict accumulator.
foldl' :: (b -> a -> b) -> b -> Patricia a -> b
foldl' f = go
  where
    go !z t =
      case t of
        Bin _ l r -> let !z' = go z l
                     in go z' r
        Tip _ a   -> f z a
        Nil       -> z

-- | \(\mathcal{O}(n)\).
--   Fold the tree left-to-right with a strict accumulator.
foldlWithKey' :: (b -> Word -> a -> b) -> b -> Patricia a -> b
foldlWithKey' f = go
  where
    go !z t =
      case t of
        Bin _ l r -> let !z' = go z l
                     in go z' r
        Tip k a   -> f z k a
        Nil       -> z



-- | \(\mathcal{O}(n_L)\).
--   Fold the tree right-to-left.
foldr :: (a -> b -> b) -> b -> Patricia a -> b
foldr f = go
  where
    go z t =
      case t of
        Bin _ l r -> go (go z r) l
        Tip _ a   -> f a z
        Nil       -> z

-- | \(\mathcal{O}(n_L)\).
--   Fold the tree right-to-left.
foldrWithKey :: (Word -> a -> b -> b) -> b -> Patricia a -> b
foldrWithKey f = go
  where
    go z t =
      case t of
        Bin _ l r -> go (go z r) l
        Tip k a   -> f k a z
        Nil       -> z

-- | \(\mathcal{O}(n)\).
--   Fold the tree right-to-left with a strict accumulator.
foldr' :: (a -> b -> b) -> b -> Patricia a -> b
foldr' f = go
  where
    go !z t =
      case t of
        Bin _ l r -> let !z' = go z r
                     in go z' l
        Tip _ a   -> f a z
        Nil       -> z

-- | \(\mathcal{O}(n)\).
--   Fold the tree right-to-left with a strict accumulator.
foldrWithKey' :: (Word -> a -> b -> b) -> b -> Patricia a -> b
foldrWithKey' f = go
  where
    go !z t =
      case t of
        Bin _ l r -> let !z' = go z r
                     in go z' l
        Tip k a   -> f k a z
        Nil       -> z



-- | \(\mathcal{O}(n_M)\).
--   Map each element in the tree to a monoid and combine the results.
foldMap :: Monoid m => (a -> m) -> Patricia a -> m
foldMap f = go
  where
    go t =
      case t of
        Bin _ l r -> go l <> go r
        Tip _ a   -> f a
        Nil       -> mempty

-- | \(\mathcal{O}(n_M)\).
--   Map each element in the tree to a monoid and combine the results.
foldMapWithKey :: Monoid m => (Word -> a -> m) -> Patricia a -> m
foldMapWithKey f = go
  where
    go t =
      case t of
        Bin _ l r -> go l <> go r
        Tip k a   -> f k a
        Nil       -> mempty



-- | \(\mathcal{O}(n)\).
--   Map each element in the tree to an action, evaluate these actions
--   left-to-right and collect the results.
traverse :: Applicative f => (a -> f b) -> Patricia a -> f (Patricia b)
traverse f = go
  where
    go t =
      case t of
        Bin p l r -> liftA2 (Bin p) (go l) (go r)
        Tip k a   -> Tip k <$> f a
        Nil       -> pure Nil

-- | \(\mathcal{O}(n)\).
--   Map each element in the tree to an action, evaluate these actions
--   left-to-right and collect the results.
traverseWithKey :: Applicative f => (Word -> a -> f b) -> Patricia a -> f (Patricia b)
traverseWithKey f = go
  where
    go t =
      case t of
        Bin p l r -> liftA2 (Bin p) (go l) (go r)
        Tip k a   -> Tip k <$> f k a
        Nil       -> pure Nil



type UBin a = (# Prefix, Patricia a, Patricia a #)

type UTip a = (# Word, a #)



-- | \(\mathcal{O}(n_A + n_B)\).
--   Unbiased union of two trees.
union :: Patricia a -> Patricia a -> Patricia a
union = anyAny
  where
    anyAny tA tB =
      case tA of
        Bin pA lA rA -> binAny (# pA, lA, rA #) tA tB

        Tip kA _ -> tipAny kA tA tB

        Nil -> tB

    tipAny kA tA tB =
      case tB of
        Bin pB lB rB -> tipBin kA tA (# pB, lB, rB #) tB

        Tip kB _
          | kA == kB  -> tA
          | otherwise -> join kA tA kB tB

        Nil -> tA

    binAny uA tA tB =
      case tB of
        Bin pB lB rB -> binBin uA tA (# pB, lB, rB #) tB

        Tip kB _ -> tipBin kB tB uA tA

        Nil -> tA

    tipBin kA tA (# pB, lB, rB #) tB
      | beyond pB kA = join kA tA pB tB
      | kA < pB      = Bin pB (tipAny kA tA lB) rB
      | otherwise    = Bin pB lB (tipAny kA tA rB)

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



-- | \(\mathcal{O}(n_A + n_B)\).
--   Left-biased union of two trees.
unionL :: Patricia a -> Patricia a -> Patricia a
unionL =
  union_ $ \s k a b ->
    let !(# c #) = case s of
                     L -> (# a #)
                     R -> (# b #)
    in Tip k c

-- | \(\mathcal{O}(n_A + n_B)\).
--   Union of two trees with a combining function.
--
--   New values are evaluated to WHNF.
unionWith'
  :: (a -> a -> a)
  -> Patricia a
  -> Patricia a
  -> Patricia a
unionWith' f =
  union_ $ \s k a b ->
    Tip k $! case s of
               L -> f a b
               R -> f b a

-- | \(\mathcal{O}(n_A + n_B)\).
--   Union of two trees with a combining function.
--
--   New values are evaluated to WHNF.
unionWithKey'
  :: (Word -> a -> a -> a)
  -> Patricia a
  -> Patricia a
  -> Patricia a
unionWithKey' f =
  union_ $ \s k a b ->
    Tip k $! case s of
               L -> f k a b
               R -> f k b a



{-# INLINE union_ #-}
union_
  :: (forall x y. S x y a a -> Key -> x -> y -> Patricia a)
  -> Patricia a
  -> Patricia a
  -> Patricia a
union_ f = anyAny L
  where
    anyAny s tA tB =
      case tA of
        Bin pA lA rA -> binAny s (# pA, lA, rA #) tA tB

        Tip kA a -> tipAny s (# kA, a #) tA tB

        Nil -> tB

    tipAny s uA@(# kA, a #) tA tB =
      case tB of
        Bin pB lB rB -> tipBin s uA tA (# pB, lB, rB #) tB

        Tip kB b
          | kA == kB  -> f s kA a b
          | otherwise -> join kA tA kB tB

        Nil -> tA

    binAny s uA tA tB =
      case tB of
        Bin pB lB rB -> binBin s uA tA (# pB, lB, rB #) tB

        Tip kB b ->
          let !(# s' #) = other s
          in tipBin s' (# kB, b #) tB uA tA

        Nil -> tA

    tipBin s uA@(# kA, _ #) tA (# pB, lB, rB #) tB
      | beyond pB kA = join kA tA pB tB
      | kA < pB      = Bin pB (tipAny s uA tA lB) rB
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



-- | \(\mathcal{O}(n_A + n_B)\).
--   Difference of two trees.
difference :: Patricia a -> Patricia b -> Patricia a
difference =
  difference_ $ \_ _ _ _ ->
    Nil

-- | \(\mathcal{O}(n_A + n_B)\).
--   Difference of two trees with a combining function.
--
--   The 'Maybe' is evaluated to WHNF.
differenceWith
  :: (a -> b -> Maybe a)
  -> Patricia a
  -> Patricia b
  -> Patricia a
differenceWith f =
  difference_ $ \s k a b ->
    retip k $ case s of
                L -> f a b
                R -> f b a

-- | \(\mathcal{O}(n_A + n_B)\).
--   Difference of two trees with a combining function.
--
--   The 'Maybe' is evaluated to WHNF.
differenceWithKey
  :: (Word -> a -> b -> Maybe a)
  -> Patricia a
  -> Patricia b
  -> Patricia a
differenceWithKey f =
  difference_ $ \s k a b ->
    retip k $ case s of
                L -> f k a b
                R -> f k b a



{-# INLINE difference_ #-}
difference_
  :: (forall x y. S x y a b -> Key -> x -> y -> Patricia a)
  -> Patricia a
  -> Patricia b
  -> Patricia a
difference_ (f :: forall n o. S n o x y -> Key -> n -> o -> Patricia x) = anyAny L
  where
    anyAny
      :: forall a b. S a b x y -> Patricia a -> Patricia b -> Patricia x
    anyAny s tA tB =
      case tA of
        Bin pA lA rA -> binAny s (# pA, lA, rA #) tA tB

        Tip kA a -> tipAny s (# kA, a #) tA tB

        Nil -> case s of
                 L -> tA
                 R -> tB

    tipAny
      :: forall a b. S a b x y -> UTip a -> Patricia a -> Patricia b -> Patricia x
    tipAny s uA@(# kA, a #) tA tB =
      case tB of
        Bin pB lB rB -> tipBin s uA tA (# pB, lB, rB #) tB

        Tip kB b
          | kA == kB  -> f s kA a b
          | otherwise -> case s of
                           L -> tA
                           R -> tB

        Nil -> case s of
                 L -> tA
                 R -> tB

    binAny
      :: forall a b. S a b x y -> UBin a -> Patricia a -> Patricia b -> Patricia x
    binAny s uA tA tB =
      case tB of
        Bin pB lB rB -> binBin s uA tA (# pB, lB, rB #) tB

        Tip kB b -> let !(# s' #) = other s
                    in tipBin s' (# kB, b #) tB uA tA

        Nil -> case s of
                 L -> tA
                 R -> tB

    tipBin
      :: forall a b. S a b x y -> UTip a -> Patricia a -> UBin b -> Patricia b -> Patricia x
    tipBin s uA@(# kA, _ #) tA (# pB, lB, rB #) tB
      | beyond pB kA = case s of
                         L -> tA
                         R -> tB

      | kA < pB      = case s of
                         L -> tipAny s uA tA lB
                         R -> rebinL pB (tipAny s uA tA lB) rB

      | otherwise    = case s of
                         L -> tipAny s uA tA rB
                         R -> rebinR pB lB (tipAny s uA tA rB)

    binBin
      :: forall a b. S a b x y -> UBin a -> Patricia a -> UBin b -> Patricia b -> Patricia x
    binBin s uA@(# pA, lA, rA #) tA uB@(# pB, lB, rB #) tB =
      let {-# NOINLINE no #-}
          no = case s of
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



-- | \(\mathcal{O}(n_A + n_B)\).
--   Compare two trees with respect to set inclusion,
--   using the given equality function for intersecting keys.
--   If any intersecting keys hold unequal values, the trees are 'Incomparable'.
compare :: (a -> b -> Bool) -> Patricia a -> Patricia b -> PartialOrdering
compare (f :: x -> y -> Bool) = anyAny L
  where
    anyAny
      :: forall a b. S a b x y -> Patricia a -> Patricia b -> PartialOrdering
    anyAny s tA tB =
      case tA of
        Bin pA lA rA -> binAny s (# pA, lA, rA #) tA tB

        Tip kA a -> tipAny s (# kA, a #) tA tB

        Nil -> case tB of
                 Nil -> Equal
                 _   -> Subset

    tipAny
      :: forall a b. S a b x y -> UTip a -> Patricia a -> Patricia b -> PartialOrdering
    tipAny s uA@(# kA, a #) tA tB =
      case tB of
        Bin pB lB rB -> tipBin s uA tA (# pB, lB, rB #)

        Tip kB b
          | kA == kB  -> let eq = case s of
                                    L -> f a b
                                    R -> f b a
                         in if eq
                              then Equal
                              else Incomparable

          | otherwise -> Incomparable

        Nil -> Superset

    binAny
      :: forall a b. S a b x y -> UBin a -> Patricia a -> Patricia b -> PartialOrdering
    binAny s uA tA tB =
      case tB of
        Bin pB lB rB -> binBin s uA tA (# pB, lB, rB #) tB

        Tip kB b -> let !(# s' #) = other s
                    in tipBin s' (# kB, b #) tB uA

        Nil -> Superset

    tipBin
      :: forall a b. S a b x y -> UTip a -> Patricia a -> UBin b -> PartialOrdering
    tipBin s uA@(# kA, _ #) tA (# pB, lB, rB #) =
      if beyond pB kA
        then Incomparable
        else limit s . tipAny s uA tA $ if kA < pB
                                           then lB
                                           else rB

    binBin
      :: forall a b. S a b x y -> UBin a -> Patricia a -> UBin b -> Patricia b -> PartialOrdering
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



-- | \(\mathcal{O}(n_A + n_B)\).
--   Determine whether two trees' key sets are disjoint.
disjoint :: Patricia a -> Patricia b -> Bool
disjoint = anyAny
  where
    anyAny tA tB =
      case tA of
        Bin pA lA rA -> binAny (# pA, lA, rA #) tA tB

        Tip kA _ -> tipAny kA tA tB

        Nil -> True

    tipAny kA tA tB =
      case tB of
        Bin pB lB rB -> tipBin kA tA (# pB, lB, rB #)

        Tip kB _ -> kA /= kB

        Nil -> True

    binAny :: forall a b. UBin a -> Patricia a -> Patricia b -> Bool
    binAny uA tA tB =
      case tB of
        Bin pB lB rB -> binBin uA tA (# pB, lB, rB #) tB

        Tip kB _ -> tipBin kB tB uA

        Nil -> True

    tipBin kA tA (# pB, lB, rB #)
      | beyond pB kA = True
      | otherwise    = tipAny kA tA $ if kA < pB
                                        then lB
                                        else rB

    binBin uA@(# pA, lA, rA #) tA uB@(# pB, lB, rB #) tB =
      case Prelude.compare pA pB of
        EQ                  -> anyAny lA lB && anyAny rA rB

        LT | pB <= upper pA -> binAny uB tB rA
           | pA >= lower pB -> binAny uA tA lB
           | otherwise      -> True

        GT | pA <= upper pB -> binAny uA tA rB
           | pB >= lower pA -> binAny uB tB lA
           | otherwise      -> True



-- | \(\mathcal{O}(n_A + n_B)\).
--   Unbiased intersection of two trees.
intersection :: Patricia a -> Patricia a -> Patricia a
intersection = anyAny
  where
    anyAny tA tB =
      case tA of
        Bin pA lA rA -> binAny (# pA, lA, rA #) tA tB

        Tip kA _ -> tipAny kA tA tB

        Nil -> Nil

    tipAny kA tA tB =
      case tB of
        Bin pB lB rB -> tipBin kA tA (# pB, lB, rB #)

        Tip kB _
          | kA == kB  -> tA
          | otherwise -> Nil

        Nil -> Nil

    binAny uA tA tB =
      case tB of
        Bin pB lB rB -> binBin uA tA (# pB, lB, rB #) tB

        Tip kB _ -> tipBin kB tB uA

        Nil -> Nil

    tipBin kA tA (# pB, lB, rB #)
      | beyond pB kA = Nil
      | otherwise    = tipAny kA tA $ if kA < pB
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



-- | \(\mathcal{O}(n_A + n_B)\).
--   Left-biased intersection of two trees.
intersectionL :: Patricia a -> Patricia b -> Patricia a
intersectionL =
  intersection_ $ \s k a b ->
    let !(# c #) = case s of
                     L -> (# a #)
                     R -> (# b #)
    in Tip k c

-- | \(\mathcal{O}(n_A + n_B)\).
--   Intersection of two trees with a combining function.
--
--   New values are evaluated to WHNF.
intersectionWith'
  :: (a -> b -> c)
  -> Patricia a
  -> Patricia b
  -> Patricia c
intersectionWith' f =
  intersection_ $ \s k a b ->
    Tip k $! case s of
               L -> f a b
               R -> f b a

-- | \(\mathcal{O}(n_A + n_B)\).
--   Intersection of two trees with a combining function.
--
--   New values are evaluated to WHNF.
intersectionWithKey'
  :: (Word -> a -> b -> c)
  -> Patricia a
  -> Patricia b
  -> Patricia c
intersectionWithKey' f =
  intersection_ $ \s k a b ->
    Tip k $! case s of
               L -> f k a b
               R -> f k b a



{-# INLINE intersection_ #-}
intersection_
  :: (forall x y. S x y a b -> Key -> x -> y -> Patricia c)
  -> Patricia a
  -> Patricia b
  -> Patricia c
intersection_ (f :: forall n o. S n o x y -> Word -> n -> o -> Patricia c) =
  anyAny L
  where
    anyAny
      :: forall a b. S a b x y -> Patricia a -> Patricia b -> Patricia c
    anyAny s tA tB =
      case tA of
        Bin pA lA rA -> binAny s (# pA, lA, rA #) tA tB

        Tip kA a -> tipAny s (# kA, a #) tA tB

        Nil -> Nil

    tipAny
      :: forall a b. S a b x y -> UTip a -> Patricia a -> Patricia b -> Patricia c
    tipAny s uA@(# kA, a #) tA tB =
      case tB of
        Bin pB lB rB -> tipBin s uA tA (# pB, lB, rB #)

        Tip kB b
          | kA == kB  -> f s kA a b
          | otherwise -> Nil

        Nil -> Nil

    binAny
      :: forall a b. S a b x y -> UBin a -> Patricia a -> Patricia b -> Patricia c
    binAny s uA tA tB =
      case tB of
        Bin pB lB rB -> binBin s uA tA (# pB, lB, rB #) tB

        Tip kB b -> let !(# s' #) = other s
                    in tipBin s' (# kB, b #) tB uA

        Nil -> Nil

    tipBin
      :: forall a b. S a b x y -> UTip a -> Patricia a -> UBin b -> Patricia c
    tipBin s uA@(# kA, _ #) tA (# pB, lB, rB #)
      | beyond pB kA = Nil
      | otherwise    = tipAny s uA tA $ if kA < pB
                                          then lB
                                          else rB

    binBin
      :: forall a b. S a b x y -> UBin a -> Patricia a -> UBin b -> Patricia b -> Patricia c
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



{-# INLINE merge #-}
-- | \(\mathcal{O}(n_A + n_B)\).
--   General merge of two trees.
--
--   Collision and single value functions __must__ return either
--   'Tip' with the respective key, or 'Nil'.
--
--   Subtree argument functions may return any tree, however the shape of said tree
--   __must__ be compatible with the prefix passed to the function.
--
--   Resulting 'Patricia' trees in argument functions are evaluated to WHNF.
--
--   This functions inlines when all argument functions are provided.
merge
  :: (Key -> a -> b -> Patricia c)                      -- ^ Single value collision
  -> (Key -> a -> Patricia c)                           -- ^ Single left value
  -> (Prefix -> Patricia a -> Patricia a -> Patricia c) -- ^ Left subtree
  -> (Key -> b -> Patricia c)                           -- ^ Single right value
  -> (Prefix -> Patricia b -> Patricia b -> Patricia c) -- ^ Right subtree
  -> Patricia a
  -> Patricia b
  -> Patricia c
merge (f :: Key -> x -> y -> Patricia c) oneX treeX oneY treeY = anyAny L
  where
    {-# INLINE side #-}
    side one tree t =
      case t of
        Bin p l r -> tree p l r
        Tip k a   -> one k a
        Nil       -> Nil

    sideX = side oneX treeX

    sideY = side oneY treeY

    sideA :: forall a b. S a b x y -> Patricia a -> Patricia c
    sideA s tA = case s of
                   L -> sideX tA
                   R -> sideY tA

    sideB :: forall a b. S a b x y -> Patricia b -> Patricia c
    sideB s tB = case s of
                   L -> sideY tB
                   R -> sideX tB

    anyAny
      :: forall a b. S a b x y -> Patricia a -> Patricia b -> Patricia c
    anyAny s tA tB =
      case tA of
        Bin pA lA rA -> binAny s (# pA, lA, rA #) tA tB

        Tip kA a     -> tipAny s (# kA, a #) tA tB

        Nil          -> sideB s tB

    tipAny
      :: forall a b. S a b x y -> UTip a -> Patricia a -> Patricia b -> Patricia c
    tipAny s uA@(# kA, a #) tA tB =
      case tB of
        Bin pB lB rB -> tipBin s uA tA (# pB, lB, rB #)

        Tip kB b
          | kA == kB  -> case s of
                           L -> f kA a b
                           R -> f kA b a

          | otherwise -> case s of
                           L -> safeJoin kA (oneX kA a) kB (sideY tB)
                           R -> safeJoin kA (oneY kA a) kB (sideX tB)

        Nil          -> sideA s tA

    binAny
      :: forall a b. S a b x y -> UBin a -> Patricia a -> Patricia b -> Patricia c
    binAny s uA tA tB =
      case tB of
        Bin pB lB rB -> binBin s uA tA (# pB, lB, rB #) tB

        Tip kB b     -> let !(# s' #) = other s
                        in tipBin s' (# kB, b #) tB uA

        Nil          -> sideA s tA

    tipBin
      :: forall a b. S a b x y -> UTip a -> Patricia a -> UBin b -> Patricia c
    tipBin s uA@(# kA, a #) tA (# pB, lB, rB #)
      | beyond pB kA = case s of
                         L -> safeJoin kA (oneX kA a) pB (treeY pB lB rB)
                         R -> safeJoin kA (oneY kA a) pB (treeX pB lB rB)

      | kA < pB      = rebin pB (tipAny s uA tA lB) (sideB s rB)

      | otherwise    = rebin pB (sideB s lB) (tipAny s uA tA rB)

    binBin
      :: forall a b. S a b x y -> UBin a -> Patricia a -> UBin b -> Patricia b -> Patricia c
    binBin s uA@(# pA, lA, rA #) tA uB@(# pB, lB, rB #) tB =
      let {-# NOINLINE no #-}
          no = case s of
                 L -> safeJoin pA (treeX pA lA rA) pB (treeY pB lB rB)
                 R -> safeJoin pA (treeY pA lA rA) pB (treeX pB lB rB)

      in case Prelude.compare pA pB of
           EQ                  -> rebin pA (anyAny s lA lB) (anyAny s rA rB)

           LT | pB <= upper pA -> let !(# s' #) = other s

                                  in rebin pA (sideA s lA) (binAny s' uB tB rA)

              | pA >= lower pB -> rebin pB (binAny s uA tA lB) (sideB s rB)

              | otherwise      -> no

           GT | pA <= upper pB -> rebin pB (sideB s lB) (binAny s uA tA rB)

              | pB >= lower pA -> let !(# s' #) = other s

                                  in rebin pA (binAny s' uB tB lA) (sideA s rA)

              | otherwise      -> no



-- | \(\mathcal{O}(\min(n,W))\).
--   Look up the value at a key in the tree.
lookup :: Word -> Patricia a -> Maybe a
lookup !w = go
  where
    go t =
      case t of
        Bin p l r
          | beyond p w -> Nothing
          | w < p      -> go l
          | otherwise  -> go r

        Tip k a
          | k == w    -> Just a
          | otherwise -> Nothing

        Nil -> Nothing

-- | \(\mathcal{O}(\min(n,W))\).
--   Look up the value at a key in the tree, falling back to the given default value
--   if it does not exist.
find :: a -> Word -> Patricia a -> a
find d !w = go
  where
    go t =
      case t of
        Bin p l r
          | beyond p w -> d
          | w < p      -> go l
          | otherwise  -> go r

        Tip k a
          | k == w    -> a
          | otherwise -> d

        Nil -> d

-- | \(\mathcal{O}(\min(n,W))\).
--   Check whether the value exists at a key in the tree.
member :: Word -> Patricia a -> Bool
member !w = go
  where
    go t =
      case t of
        Bin p l r
          | beyond p w -> False
          | w < p      -> go l
          | otherwise  -> go r

        Tip k _ -> k == w

        Nil -> False

-- 'lookup' that doesn't allocate a 'Maybe'.
takeOne :: Word -> Patricia a -> Patricia a
takeOne !w = go
  where
    go t =
      case t of
        Bin p l r
          | beyond p w -> Nil
          | w < p      -> go l
          | otherwise  -> go r

        Tip k _
          | k == w    -> t
          | otherwise -> Nil

        Nil -> Nil



-- | \(\mathcal{O}(\min(n,W))\).
--   Look up the value at a key in the tree.
dirtyLookup :: Word -> Patricia a -> Maybe a
dirtyLookup !w = go
  where
    go t =
      case t of
        Bin p l r
          | w < p     -> go l
          | otherwise -> go r

        Tip k a
          | k == w    -> Just a
          | otherwise -> Nothing

        Nil -> Nothing

-- | \(\mathcal{O}(\min(n,W))\).
--   Look up the value at a key in the tree, falling back to the default value
--   if it does not exist.
dirtyFind :: a -> Word -> Patricia a -> a
dirtyFind d !w = go
  where
    go t =
      case t of
        Bin p l r
          | w < p     -> go l
          | otherwise -> go r

        Tip k a
          | k == w    -> a
          | otherwise -> d

        Nil -> d

-- | \(\mathcal{O}(\min(n,W))\).
--   Check whether the value exists at a key in the tree.
dirtyMember :: Word -> Patricia a -> Bool
dirtyMember !w = go
  where
    go t =
      case t of
        Bin p l r
          | w < p     -> go l
          | otherwise -> go r

        Tip k _ -> k == w

        Nil -> False



-- | \(\mathcal{O}(\min(n,W))\).
--   Insert a new value in the tree at the given key.
--   If a value already exists at that key, it is replaced.
insert :: Word -> a -> Patricia a -> Patricia a
insert !w a = go
  where
    go t =
      case t of
        Bin p l r
          | beyond p w -> join w (Tip w a) p t
          | w < p      -> Bin p (go l) r
          | otherwise  -> Bin p l (go r)

        Tip k _
          | k == w    -> Tip k a
          | otherwise -> join w (Tip w a) k t

        Nil -> Tip w a



-- | \(\mathcal{O}(\min(n,W))\).
--   Insert a new value in the tree at the given key.
--   If a value already exists at that key, the function is used instead.
insertWith :: (a -> a) -> Word -> a -> Patricia a -> Patricia a
insertWith f !w b = go
  where
    go t =
      case t of
        Bin p l r
          | beyond p w -> join w (Tip w b) p t
          | w < p      -> Bin p (go l) r
          | otherwise  -> Bin p l (go r)

        Tip k a
          | k == w    -> Tip k (f a)
          | otherwise -> join w (Tip w b) k t

        Nil -> Tip w b

-- | \(\mathcal{O}(\min(n,W))\).
--   Insert a new value in the tree at the given key.
--   If a value already exists at that key, the function is used instead.
--
--   New value is evaluted to WHNF.
insertWith' :: (a -> a) -> Word -> a -> Patricia a -> Patricia a
insertWith' f !w b = go
  where
    go t =
      case t of
        Bin p l r
          | beyond p w -> join w (b `seq` Tip w b) p t
          | w < p      -> Bin p (go l) r
          | otherwise  -> Bin p l (go r)

        Tip k a
          | k == w    -> Tip k $! f a
          | otherwise -> join w (b `seq` Tip w b) k t

        Nil -> Tip w b



-- | \(\mathcal{O}(\min(n,W))\).
--   Apply a function to a value in the tree at the given key.
adjust :: (a -> a) -> Word -> Patricia a -> Patricia a
adjust f !w = go
  where
    go t =
      case t of
        Bin p l r
          | beyond p w -> t
          | w < p      -> Bin p (go l) r
          | otherwise  -> Bin p l (go r)

        Tip k a
          | k == w    -> Tip k (f a)
          | otherwise -> t

        Nil -> Nil

-- | \(\mathcal{O}(\min(n,W))\).
--   Apply a function to a value in the tree at the given key.
--
--   New value is evaluated to WHNF.
adjust' :: (a -> a) -> Word -> Patricia a -> Patricia a
adjust' f !w = go
  where
    go t =
      case t of
        Bin p l r
          | beyond p w -> t
          | w < p      -> Bin p (go l) r
          | otherwise  -> Bin p l (go r)

        Tip k a
          | k == w    -> Tip k $! f a
          | otherwise -> t

        Nil -> Nil



-- | \(\mathcal{O}(\min(n,W))\).
--   Delete a value in the tree at the given key.
delete :: Word -> Patricia a -> Patricia a
delete !w = go
  where
    go t =
      case t of
        Bin p l r
          | beyond p w -> t
          | w < p      -> rebinL p (go l) r
          | otherwise  -> rebinR p l (go r)

        Tip k _
          | k == w    -> Nil
          | otherwise -> t

        Nil -> Nil

-- | \(\mathcal{O}(\min(n,W))\).
--   Update or delete a value in the tree at the given key.
--
--   The 'Maybe' is evaluated to WHNF.
update :: (a -> Maybe a) -> Word -> Patricia a -> Patricia a
update f !w = go
  where
    go t =
      case t of
        Bin p l r
          | beyond p w -> t
          | w < p      -> rebinL p (go l) r
          | otherwise  -> rebinR p l (go r)

        Tip k a
          | k == w    -> retip k (f a)
          | otherwise -> t

        Nil -> Nil



-- | \(\mathcal{O}(\min(n,W))\).
--   Insert, update or delete a value in the tree at the given key.
--
--   The resulting 'Maybe' is evaluated to WHNF.
alter :: (Maybe a -> Maybe a) -> Word -> Patricia a -> Patricia a
alter f !w = go
  where
    go t =
      case t of
        Bin p l r
          | beyond p w -> case f Nothing of
                            Just b  -> join p t w (Tip w b)
                            Nothing -> t

          | w < p      -> rebinL p (go l) r
          | otherwise  -> rebinR p l (go r)

        Tip k a
          | k == w    -> case f (Just a) of
                           Just b  -> Tip k b
                           Nothing -> Nil

          | otherwise -> case f Nothing of
                           Just b  -> join k t w (Tip w b)
                           Nothing -> t

        Nil -> case f Nothing of
                 Just b  -> Tip w b
                 Nothing -> Nil



-- | \(\mathcal{O}(\min(n,W))\).
--   Look up a value at a largest key smaller than or equal to the given key.
lookupL :: Word -> Patricia a -> Maybe (Lookup a)
lookupL !w = go
  where
    go t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then go l
                   else Nothing

            else Just $! if w <= upper p
                           then case go r of
                                  Just x  -> x
                                  Nothing -> unsafeLookupMaxWithKey l

                           else unsafeLookupMaxWithKey r

        Tip k a
          | k <= w    -> Just $! Lookup k a
          | otherwise -> Nothing

        Nil -> Nothing

-- | \(\mathcal{O}(\min(n,W))\).
--   Look up a value at a smallest key greater than or equal to the given key.
lookupR :: Word -> Patricia a -> Maybe (Lookup a)
lookupR !w = go
  where
    go t =
      case t of
        Bin p l r ->
          if w < p
            then Just $! if w >= lower p
                           then case go l of
                                  Just x  -> x
                                  Nothing -> unsafeLookupMinWithKey r

                           else unsafeLookupMinWithKey l

            else if w <= upper p
                   then go r
                   else Nothing

        Tip k a
          | k >= w    -> Just $! Lookup k a
          | otherwise -> Nothing

        Nil -> Nothing



-- | \(\mathcal{O}(\min(n,W) + n_L)\).
--   Apply a function to every value for which the key is smaller than
--   or equal to the given one.
adjustL :: (a -> a) -> Word -> Patricia a -> Patricia a
adjustL f !w = go
  where
    go t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then Bin p (go l) r
                   else t

            else Bin p (Data.Patricia.Word.Strict.Internal.map f l) $
                   if w <= upper p
                     then go r
                     else Data.Patricia.Word.Strict.Internal.map f r

        Tip k a
          | k <= w    -> Tip k (f a)
          | otherwise -> t

        Nil         -> Nil

-- | \(\mathcal{O}(\min(n,W) + n_L)\).
--   Apply a function to every value for which the key is smaller than
--   or equal to the given one.
--
--   New value is evaluated to WHNF.
adjustL' :: (a -> a) -> Word -> Patricia a -> Patricia a
adjustL' f !w = go
  where
    go t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then Bin p (go l) r
                   else t

            else Bin p (map' f l) $
                   if w <= upper p
                     then go r
                     else map' f r

        Tip k a
          | k <= w    -> Tip k $! f a
          | otherwise -> t

        Nil         -> Nil

-- | \(\mathcal{O}(\min(n,W) + n_L)\).
--   Apply a function to every value for which the key is smaller than
--   or equal to the given one.
adjustLWithKey :: (Word -> a -> a) -> Word -> Patricia a -> Patricia a
adjustLWithKey f !w = go
  where
    go t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then Bin p (go l) r
                   else t

            else Bin p (mapWithKey f l) $
                   if w <= upper p
                     then go r
                     else mapWithKey f r

        Tip k a
          | k <= w    -> Tip k (f k a)
          | otherwise -> t

        Nil         -> Nil

-- | \(\mathcal{O}(\min(n,W) + n_L)\).
--   Apply a function to every value for which the key is smaller than
--   or equal to the given one.
--
--   New value is evaluated to WHNF.
adjustLWithKey' :: (Word -> a -> a) -> Word -> Patricia a -> Patricia a
adjustLWithKey' f !w = go
  where
    go t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then Bin p (go l) r
                   else t

            else Bin p (mapWithKey' f l) $
                   if w <= upper p
                     then go r
                     else mapWithKey' f r

        Tip k a
          | k <= w    -> Tip k $! f k a
          | otherwise -> t

        Nil         -> Nil


-- | \(\mathcal{O}(\min(n,W))\).
--   Delete values for which keys are smaller than or equal to the given one.
deleteL :: Word -> Patricia a -> Patricia a
deleteL !w = go
  where
    go t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then rebinL p (go l) r
                   else t

            else if w <= upper p
                   then go r
                   else Nil

        Tip k _
          | k <= w    -> Nil
          | otherwise -> t

        Nil         -> Nil


-- | \(\mathcal{O}(\min(n,W) + n_L)\).
--   Update every value for which the key is smaller than or equal to the given one.
--
--   The 'Maybe' is evaluated to WHNF.
updateL :: (a -> Maybe a) -> Word -> Patricia a -> Patricia a
updateL f !w = go
  where
    go t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then rebinL p (go l) r
                   else t

            else rebin p (mapMaybe f l) $
                   if w <= upper p
                     then go r
                     else mapMaybe f r

        Tip k a
          | k <= w    -> retip k (f a)
          | otherwise -> t

        Nil         -> Nil

-- | \(\mathcal{O}(\min(n,W) + n_L)\).
--   Update every value for which the key is smaller than or equal to the given one.
--
--   The 'Maybe' is evaluated to WHNF.
updateLWithKey :: (Word -> a -> Maybe a) -> Word -> Patricia a -> Patricia a
updateLWithKey f !w = go
  where
    go t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then rebinL p (go l) r
                   else t

            else rebin p (mapMaybeWithKey f l) $
                   if w <= upper p
                     then go r
                     else mapMaybeWithKey f r

        Tip k a
          | k <= w    -> retip k (f k a)
          | otherwise -> t

        Nil         -> Nil


-- | \(\mathcal{O}(\min(n,W))\).
--   Take values for which keys are smaller than or equal to the given one.
takeL :: Word -> Patricia a -> Patricia a
takeL !w = go
  where
    go t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then go l
                   else Nil

            else if w <= upper p
                   then rebinR p l (go r)
                   else t

        Tip k _
          | k <= w    -> t
          | otherwise -> Nil

        Nil -> Nil



-- | \(\mathcal{O}(\min(n,W) + n_R)\).
--   Apply a function to every value for which the key is greater than
--   or equal to the given one.
adjustR :: (a -> a) -> Word -> Patricia a -> Patricia a
adjustR f !w = go
  where
    go t =
      case t of
        Bin p l r ->
          if w < p
            then let l' = if w >= lower p
                            then go l
                            else Data.Patricia.Word.Strict.Internal.map f l

                 in Bin p l' (Data.Patricia.Word.Strict.Internal.map f r)

            else if w <= upper p
                   then Bin p l (go r)
                   else t

        Tip k a
          | k >= w    -> Tip k (f a)
          | otherwise -> t

        Nil         -> Nil

-- | \(\mathcal{O}(\min(n,W) + n_R)\).
--   Apply a function to every value for which the key is greater than
--   or equal to the given one.
--
--   New value is evaluated to WHNF.
adjustR' :: (a -> a) -> Word -> Patricia a -> Patricia a
adjustR' f !w = go
  where
    go t =
      case t of
        Bin p l r ->
          if w < p
            then let l' = if w >= lower p
                            then go l
                            else map' f l

                 in Bin p l' (map' f r)

            else if w <= upper p
                   then Bin p l (go r)
                   else t

        Tip k a
          | k >= w    -> Tip k $! f a
          | otherwise -> t

        Nil         -> Nil

-- | \(\mathcal{O}(\min(n,W) + n_R)\).
--   Apply a function to every value for which the key is greater than
--   or equal to the given one.
adjustRWithKey :: (Word -> a -> a) -> Word -> Patricia a -> Patricia a
adjustRWithKey f !w = go
  where
    go t =
      case t of
        Bin p l r ->
          if w < p
            then let l' = if w >= lower p
                            then go l
                            else mapWithKey f l

                 in Bin p l' (mapWithKey f r)

            else if w <= upper p
                   then Bin p l (go r)
                   else t

        Tip k a
          | k >= w    -> Tip k (f k a)
          | otherwise -> t

        Nil         -> Nil

-- | \(\mathcal{O}(\min(n,W) + n_R)\).
--   Apply a function to every value for which the key is greater than
--   or equal to the given one.
--
--   New value is evaluated to WHNF.
adjustRWithKey' :: (Word -> a -> a) -> Word -> Patricia a -> Patricia a
adjustRWithKey' f !w = go
  where
    go t =
      case t of
        Bin p l r ->
          if w < p
            then let l' = if w >= lower p
                            then go l
                            else mapWithKey' f l

                 in Bin p l' (mapWithKey' f r)

            else if w <= upper p
                   then Bin p l (go r)
                   else t

        Tip k a
          | k >= w    -> Tip k $! f k a
          | otherwise -> t

        Nil         -> Nil


-- | \(\mathcal{O}(\min(n,W))\).
--   Delete values for which keys are greater than or equal to the given one.
deleteR :: Word -> Patricia a -> Patricia a
deleteR !w = go
  where
    go t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then go l
                   else Nil

            else if w <= upper p
                   then rebinR p l (go r)
                   else t

        Tip k _
          | k >= w    -> Nil
          | otherwise -> t

        Nil         -> Nil


-- | \(\mathcal{O}(\min(n,W) + n_R)\).
--   Update every value for which the key is greater than or equal to the given one.
--
--   The 'Maybe' is evaluated to WHNF.
updateR :: (a -> Maybe a) -> Word -> Patricia a -> Patricia a
updateR f !w = go
  where
    go t =
      case t of
        Bin p l r ->
          if w < p
            then let l' = if w >= lower p
                            then go l
                            else mapMaybe f l

                 in rebin p l' (mapMaybe f r)

            else if w <= upper p
                   then rebinR p l (go r)
                   else t

        Tip k a
          | k >= w    -> retip k (f a)
          | otherwise -> t

        Nil         -> Nil

-- | \(\mathcal{O}(\min(n,W) + n_R)\).
--   Update every value for which the key is greater than or equal to the given one.
--
--   The 'Maybe' is evaluated to WHNF.
updateRWithKey :: (Word -> a -> Maybe a) -> Word -> Patricia a -> Patricia a
updateRWithKey f !w = go
  where
    go t =
      case t of
        Bin p l r ->
          if w < p
            then let l' = if w >= lower p
                            then go l
                            else mapMaybeWithKey f l

                 in rebin p l' (mapMaybeWithKey f r)

            else if w <= upper p
                   then rebinR p l (go r)
                   else t

        Tip k a
          | k >= w    -> retip k (f k a)
          | otherwise -> t

        Nil         -> Nil


-- | \(\mathcal{O}(\min(n,W))\).
--   Take values for which keys are greater than or equal to the given one.
takeR :: Word -> Patricia a -> Patricia a
takeR !w = go
  where
    go t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then rebinL p (go l) r
                   else t

            else if w <= upper p
                   then go r
                   else Nil

        Tip k _
          | k >= w    -> t
          | otherwise -> Nil

        Nil -> Nil



-- | \(\mathcal{O}(\min(n,W) + n_I)\).
--   Apply a function to every value for which the key is in the given range.
adjustRange :: (a -> a) -> Range -> Patricia a -> Patricia a
adjustRange f (UnsafeRange kL kR)
  | kL == kR  = adjust f kL
  | otherwise = unsafeAdjustRange f kL kR

-- | \(\mathcal{O}(\min(n,W) + n_I)\).
--   Apply a function to every value for which the key is in the given range.
--
--   \(k_R\) __must__ be greater than \(k_L\).
unsafeAdjustRange
  :: (a -> a)
  -> Word     -- ^ \(k_L\)
  -> Word     -- ^ \(k_R\)
  -> Patricia a
  -> Patricia a
unsafeAdjustRange f !wL !wR = go
  where
    !mM = branchingBit wL wR

    !pM = mask wL mM .|. mM

    go t =
      case t of
        Bin p l r ->
          case Prelude.compare p pM of
            EQ                 -> Bin p (adjustR f wL l) (adjustL f wR r)

            LT | pM <= upper p -> Bin p l (go r)
               | p >= lower pM -> if wL < p
                                    then Bin p
                                           (adjustR f wL l)
                                           (Data.Patricia.Word.Strict.Internal.map f r)

                                    else Bin p l (adjustR f wL r)

               | otherwise     -> t

            GT | p <= upper pM -> if wR >= p
                                    then Bin p
                                           (Data.Patricia.Word.Strict.Internal.map f l)
                                           (adjustL f wR r)

                                    else Bin p (adjustL f wR l) r

               | pM >= lower p -> Bin p (go l) r
               | otherwise     -> t

        Tip k a
          | k >= wL && k <= wR -> Tip k (f a)
          | otherwise          -> t

        Nil -> Nil

-- | \(\mathcal{O}(\min(n,W) + n_I)\).
--   Apply a function to every value for which the key is in the given range.
--
--   New value is evaluated to WHNF.
adjustRange' :: (a -> a) -> Range -> Patricia a -> Patricia a
adjustRange' f (UnsafeRange kL kR)
  | kL == kR  = adjust' f kL
  | otherwise = unsafeAdjustRange' f kL kR

-- | \(\mathcal{O}(\min(n,W) + n_I)\).
--   Apply a function to every value for which the key is in the given range.
--
--   New value is evaluated to WHNF.
--
--   \(k_R\) __must__ be greater than \(k_L\).
unsafeAdjustRange'
  :: (a -> a)
  -> Word     -- ^ \(k_L\)
  -> Word     -- ^ \(k_R\)
  -> Patricia a
  -> Patricia a
unsafeAdjustRange' f !wL !wR = go
  where
    !mM = branchingBit wL wR

    !pM = mask wL mM .|. mM

    go t =
      case t of
        Bin p l r ->
          case Prelude.compare p pM of
            EQ                 -> Bin p (adjustR' f wL l) (adjustL' f wR r)

            LT | pM <= upper p -> Bin p l (go r)
               | p >= lower pM -> if wL < p
                                    then Bin p (adjustR' f wL l) (map' f r)
                                    else Bin p l (adjustR' f wL r)

               | otherwise     -> t

            GT | p <= upper pM -> if wR >= p
                                    then Bin p (map' f l) (adjustL' f wR r)
                                    else Bin p (adjustL' f wR l) r

               | pM >= lower p -> Bin p (go l) r
               | otherwise     -> t

        Tip k a
          | k >= wL && k <= wR -> Tip k $! f a
          | otherwise          -> t

        Nil -> Nil

-- | \(\mathcal{O}(\min(n,W) + n_I)\).
--   Apply a function to every value for which the key is in the given range.
adjustRangeWithKey :: (Word -> a -> a) -> Range -> Patricia a -> Patricia a
adjustRangeWithKey f (UnsafeRange kL kR)
  | kL == kR  = adjust (f kL) kL
  | otherwise = unsafeAdjustRangeWithKey f kL kR

-- | \(\mathcal{O}(\min(n,W) + n_I)\).
--   Apply a function to every value for which the key is in the given range.
--
--   \(k_R\) __must__ be greater than \(k_L\).
unsafeAdjustRangeWithKey
  :: (Word -> a -> a)
  -> Word             -- ^ \(k_L\)
  -> Word             -- ^ \(k_R\)
  -> Patricia a
  -> Patricia a
unsafeAdjustRangeWithKey f !wL !wR = go
  where
    !mM = branchingBit wL wR

    !pM = mask wL mM .|. mM

    go t =
      case t of
        Bin p l r ->
          case Prelude.compare p pM of
            EQ                 -> Bin p (adjustRWithKey f wL l) (adjustLWithKey f wR r)

            LT | pM <= upper p -> Bin p l (go r)
               | p >= lower pM -> if wL < p
                                    then Bin p (adjustRWithKey f wL l) (mapWithKey f r)
                                    else Bin p l (adjustRWithKey f wL r)

               | otherwise     -> t

            GT | p <= upper pM -> if wR >= p
                                    then Bin p (mapWithKey f l) (adjustLWithKey f wR r)
                                    else Bin p (adjustLWithKey f wR l) r

               | pM >= lower p -> Bin p (go l) r
               | otherwise     -> t

        Tip k a
          | k >= wL && k <= wR -> Tip k (f k a)
          | otherwise          -> t

        Nil -> Nil

-- | \(\mathcal{O}(\min(n,W) + n_I)\).
--   Apply a function to every value for which the key is in the given range.
--
--   New value is evaluated to WHNF.
adjustRangeWithKey' :: (Word -> a -> a) -> Range -> Patricia a -> Patricia a
adjustRangeWithKey' f (UnsafeRange kL kR)
  | kL == kR  = adjust' (f kL) kL
  | otherwise = unsafeAdjustRangeWithKey' f kL kR

-- | \(\mathcal{O}(\min(n,W) + n_I)\).
--   Apply a function to every value for which the key is in the given range.
--
--   New value is evaluated to WHNF.
--
--   \(k_R\) __must__ be greater than \(k_L\).
unsafeAdjustRangeWithKey'
  :: (Word -> a -> a)
  -> Word             -- ^ \(k_L\)
  -> Word             -- ^ \(k_R\)
  -> Patricia a
  -> Patricia a
unsafeAdjustRangeWithKey' f !wL !wR = go
  where
    !mM = branchingBit wL wR

    !pM = mask wL mM .|. mM

    go t =
      case t of
        Bin p l r ->
          case Prelude.compare p pM of
            EQ                 -> Bin p (adjustRWithKey' f wL l) (adjustLWithKey' f wR r)

            LT | pM <= upper p -> Bin p l (go r)
               | p >= lower pM -> if wL < p
                                    then Bin p (adjustRWithKey' f wL l) (mapWithKey' f r)
                                    else Bin p l (adjustRWithKey' f wL r)

               | otherwise     -> t

            GT | p <= upper pM -> if wR >= p
                                    then Bin p (mapWithKey' f l) (adjustLWithKey' f wR r)
                                    else Bin p (adjustLWithKey' f wR l) r

               | pM >= lower p -> Bin p (go l) r
               | otherwise     -> t

        Tip k a
          | k >= wL && k <= wR -> Tip k $! f k a
          | otherwise          -> t

        Nil -> Nil


-- | \(\mathcal{O}(\min(n,W))\).
--   Delete values for which keys are in the given range.
deleteRange :: Range -> Patricia a -> Patricia a
deleteRange (UnsafeRange kL kR)
  | kL == kR  = delete kL
  | otherwise = unsafeDeleteRange kL kR

-- | \(\mathcal{O}(\min(n,W))\).
--   Delete values for which keys are in the given range.
--
--   \(k_R\) __must__ be greater than \(k_L\).
unsafeDeleteRange
  :: Word         -- ^ \(k_L\)
  -> Word         -- ^ \(k_R\)
  -> Patricia a
  -> Patricia a
unsafeDeleteRange !wL !wR = go
  where
    !mM = branchingBit wL wR

    !pM = mask wL mM .|. mM

    go t =
      case t of
        Bin p l r ->
          case Prelude.compare p pM of
            EQ                 -> rebin p (deleteR wL l) (deleteL wR r)

            LT | pM <= upper p -> rebinR p l (go r)
               | p >= lower pM -> if wL < p
                                    then deleteR wL l
                                    else rebinR p l (deleteR wL r)
               | otherwise     -> t

            GT | p <= upper pM -> if wR >= p
                                    then deleteL wR r
                                    else rebinL p (deleteL wR l) r

               | pM >= lower p -> rebinL p (go l) r
               | otherwise     -> t

        Tip k _
          | k >= wL && k <= wR -> Nil
          | otherwise          -> t

        Nil -> Nil


-- | \(\mathcal{O}(\min(n,W) + n_I)\).
--   Update every value for which the key is in the given range.
--
--   The 'Maybe' is evaluated to WHNF.
updateRange :: (a -> Maybe a) -> Range -> Patricia a -> Patricia a
updateRange f (UnsafeRange kL kR)
  | kL == kR  = update f kL
  | otherwise = unsafeUpdateRange f kL kR

-- | \(\mathcal{O}(\min(n,W) + n_I)\).
--   Update every value for which the key is in the given range.
--
--   The 'Maybe' is evaluated to WHNF.
--
--   \(k_R\) __must__ be greater than \(k_L\).
unsafeUpdateRange
  :: (a -> Maybe a)
  -> Word           -- ^ \(k_L\)
  -> Word           -- ^ \(k_R\)
  -> Patricia a
  -> Patricia a
unsafeUpdateRange f !wL !wR = go
  where
    !mM = branchingBit wL wR

    !pM = mask wL mM .|. mM

    go t =
      case t of
        Bin p l r ->
          case Prelude.compare p pM of
            EQ                 -> rebin p (updateR f wL l) (updateL f wR r)

            LT | pM <= upper p -> rebinR p l (go r)
               | p >= lower pM -> if wL < p
                                    then rebinL p (updateR f wL l) (mapMaybe f r)
                                    else rebinR p l (updateR f wL r)
               | otherwise     -> t

            GT | p <= upper pM -> if wR >= p
                                    then rebinR p (mapMaybe f l) (updateL f wR r)
                                    else rebinL p (updateL f wR l) r

               | pM >= lower p -> rebinL p (go l) r
               | otherwise     -> t

        Tip k a
          | k >= wL && k <= wR -> retip k (f a)
          | otherwise          -> t

        Nil -> Nil

-- | \(\mathcal{O}(\min(n,W) + n_I)\).
--   Update every value for which the key is in the given range.
--
--   The 'Maybe' is evaluated to WHNF.
updateRangeWithKey :: (Word -> a -> Maybe a) -> Range -> Patricia a -> Patricia a
updateRangeWithKey f (UnsafeRange kL kR)
  | kL == kR  = update (f kL) kL
  | otherwise = unsafeUpdateRangeWithKey f kL kR

-- | \(\mathcal{O}(\min(n,W) + n_I)\).
--   Update every value for which the key is in the given range.
--
--   The 'Maybe' is evaluated to WHNF.
--
--   \(k_R\) __must__ be greater than \(k_L\).
unsafeUpdateRangeWithKey
  :: (Word -> a -> Maybe a)
  -> Word                   -- ^ \(k_L\)
  -> Word                   -- ^ \(k_R\)
  -> Patricia a
  -> Patricia a
unsafeUpdateRangeWithKey f !wL !wR = go
  where
    !mM = branchingBit wL wR

    !pM = mask wL mM .|. mM

    go t =
      case t of
        Bin p l r ->
          case Prelude.compare p pM of
            EQ                 -> rebin p (updateRWithKey f wL l) (updateLWithKey f wR r)

            LT | pM <= upper p -> rebinR p l (go r)
               | p >= lower pM -> if wL < p
                                    then rebinL p (updateRWithKey f wL l)
                                                  (mapMaybeWithKey f r)

                                    else rebinR p l (updateRWithKey f wL r)
               | otherwise     -> t

            GT | p <= upper pM -> if wR >= p
                                    then rebinR p (mapMaybeWithKey f l)
                                                  (updateLWithKey f wR r)

                                    else rebinL p (updateLWithKey f wR l) r

               | pM >= lower p -> rebinL p (go l) r
               | otherwise     -> t

        Tip k a
          | k >= wL && k <= wR -> retip k (f k a)
          | otherwise          -> t

        Nil -> Nil



-- | \(\mathcal{O}(\min(n,W))\).
--   Take values for which keys are in the given range.
takeRange :: Range -> Patricia a -> Patricia a
takeRange (UnsafeRange kL kR)
  | kL == kR  = takeOne kL
  | otherwise = unsafeTakeRange kL kR

-- | \(\mathcal{O}(\min(n,W))\).
--   Take values for which keys are in the given range.
--
--   \(k_R\) __must__ be greater than \(k_L\).
unsafeTakeRange
  :: Word       -- ^ \(k_L\)
  -> Word       -- ^ \(k_R\)
  -> Patricia a
  -> Patricia a
unsafeTakeRange !wL !wR = go
  where
    !mM = branchingBit wL wR

    !pM = mask wL mM .|. mM

    go t =
      case t of
        Bin p l r ->
          case Prelude.compare p pM of
            EQ                 -> rebin p (takeR wL l) (takeL wR r)

            LT | pM <= upper p -> go r
               | p >= lower pM -> if wL < p
                                    then rebinL p (takeR wL l) r
                                    else takeR wL r

               | otherwise     -> Nil

            GT | p <= upper pM -> if wR >= p
                                    then rebinR p l (takeL wR r)
                                    else takeL wR l

               | pM >= lower p -> go l
               | otherwise     -> Nil

        Tip k _
          | k >= wL && k <= wR -> t
          | otherwise          -> Nil

        Nil -> Nil



-- | Result of a tree split.
data Split l r = Split !(Patricia l) !(Patricia r)
                 deriving Show

-- | \(\mathcal{O}(\min(n,W))\).
--   Split the tree into two, such that
--   values with keys smaller than or equal to the given one are on the left,
--   and values with keys greater than the given one are on the right.
splitL :: Word -> Patricia a -> Split a a
splitL !w = \t ->
  case go t of
    (# !l, !r #) -> Split l r
  where
    go t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then let !(# !ll, !lr #) = go l
                        in (# ll, rebinL p lr r #)

                   else (# Nil, t #)

            else if w <= upper p
                   then let !(# !rl, !rr #) = go r
                        in (# rebinR p l rl, rr #)

                   else (# t, Nil #)

        Tip k _
          | w >= k    -> (# t, Nil #)
          | otherwise -> (# Nil, t #)

        Nil -> (# Nil, Nil #)

-- | \(\mathcal{O}(\min(n,W))\).
--   Split the tree into two, such that
--   values with keys smaller than the given one are on the left,
--   and values with keys greater than or equal to the given one are on the right.
splitR :: Word -> Patricia a -> Split a a
splitR !w = \t ->
  case go t of
    (# !l, !r #) -> Split l r
  where
    go t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then let !(# !ll, !lr #) = go l
                        in (# ll, rebinL p lr r #)

                   else (# Nil, t #)

            else if w <= upper p
                   then let !(# !rl, !rr #) = go r
                        in (# rebinR p l rl, rr #)

                   else (# t, Nil #)

        Tip k _
          | w > k     -> (# t, Nil #)
          | otherwise -> (# Nil, t #)

        Nil -> (# Nil, Nil #)



-- | Result of a tree split with a lookup.
data SplitLookup l x r = SplitLookup !(Patricia l) !(Maybe x) !(Patricia r)
                         deriving Show

-- | \(\mathcal{O}(\min(n,W))\).
--   Split the tree into two, such that
--   values with keys smaller than the given one are on the left,
--   values with keys greater than the given one are on the right,
--   and the value at the given key is returned separately.
splitLookup :: Word -> Patricia a -> SplitLookup a a a
splitLookup !w = \t ->
  case go t of
    (# !l, !mx, !r #) -> SplitLookup l mx r
  where
    go t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then let !(# !ll, !mx, !lr #) = go l
                        in (# ll, mx, rebinL p lr r #)

                   else (# Nil, Nothing, t #)

            else if w <= upper p
                   then let !(# !rl, !mx, !rr #) = go r
                        in (# rebinR p l rl, mx, rr #)

                   else (# t, Nothing, Nil #)

        Tip k a ->
          case w `Prelude.compare` k of
            EQ -> (# Nil, Just a , Nil #)
            GT -> (# t  , Nothing, Nil #)
            LT -> (# Nil, Nothing, t   #)

        Nil -> (# Nil, Nothing, Nil #)



-- | \(\mathcal{O}(n)\).
--   Filter values that satisfy the value predicate.
filter :: (a -> Bool) -> Patricia a -> Patricia a
filter f = go
  where
    go t =
      case t of
        Bin p l r -> rebin p (go l) (go r)

        Tip _ a
          | f a       -> t
          | otherwise -> Nil

        Nil -> Nil

-- | \(\mathcal{O}(n)\).
--   Filter values that satisfy the value predicate.
filterWithKey :: (Word -> a -> Bool) -> Patricia a -> Patricia a
filterWithKey f = go
  where
    go t =
      case t of
        Bin p l r -> rebin p (go l) (go r)

        Tip k a
          | f k a     -> t
          | otherwise -> Nil

        Nil -> Nil



-- | \(\mathcal{O}(n)\).
--   Apply a function to every value in the tree and create one out of 'Just' values.
--
--   The 'Maybe' is evaluated to WHNF.
mapMaybe :: (a -> Maybe b) -> Patricia a -> Patricia b
mapMaybe f = go
  where
    go t =
      case t of
        Bin p l r -> rebin p (go l) (go r)

        Tip k a ->
          case f a of
            Just b  -> Tip k b
            Nothing -> Nil

        Nil -> Nil

-- | \(\mathcal{O}(n)\).
--   Apply a function to every value in the tree
--   and create a tree out of 'Just' results.
--
--   The 'Maybe' is evaluated to WHNF.
mapMaybeWithKey :: (Word -> a -> Maybe b) -> Patricia a -> Patricia b
mapMaybeWithKey f = go
  where
    go t =
      case t of
        Bin p l r -> rebin p (go l) (go r)

        Tip k a ->
          case f k a of
            Just b  -> Tip k b
            Nothing -> Nil

        Nil -> Nil



-- | \(\mathcal{O}(n)\).
--   Split the tree into two, such that values that satisfy the predicate
--   are on the left and values that do not are on the right.
partition :: (a -> Bool) -> Patricia a -> Split a a
partition f = \t ->
  case go t of
    (# !l, !r #) -> Split l r
  where
    go t =
      case t of
        Bin p l r ->
          let !(# !ll, !lr #) = go l
              !(# !rl, !rr #) = go r

          in (# rebin p ll rl, rebin p lr rr #)

        Tip _ a
          | f a       -> (# t, Nil #)
          | otherwise -> (# Nil, t #)

        Nil -> (# Nil, Nil #)

-- | \(\mathcal{O}(n)\).
--   Split the tree into two, such that values that satisfy the predicate
--   are on the left and values that do not are on the right.
partitionWithKey :: (Word -> a -> Bool) -> Patricia a -> Split a a
partitionWithKey f = \t ->
  case go t of
    (# !l, !r #) -> Split l r
  where
    go t =
      case t of
        Bin p l r ->
          let !(# !ll, !lr #) = go l
              !(# !rl, !rr #) = go r

          in (# rebin p ll rl, rebin p lr rr #)

        Tip k a
          | f k a     -> (# t, Nil #)
          | otherwise -> (# Nil, t #)

        Nil -> (# Nil, Nil #)


-- | \(\mathcal{O}(n)\).
--   Apply a function to every value in the tree and create two trees,
--   one out of 'Left' results and one out of 'Right' ones.
--
--   The 'Either' is evaluated to WHNF.
mapEither :: (a -> Either b c) -> Patricia a -> Split b c
mapEither f = \t ->
  case go t of
    (# !l, !r #) -> Split l r
  where
    go t =
      case t of
        Bin p l r ->
          let !(# !ll, !lr #) = go l
              !(# !rl, !rr #) = go r

          in (# rebin p ll rl, rebin p lr rr #)

        Tip k a ->
          case f a of
            Left b  -> (# Tip k b, Nil #)
            Right c -> (# Nil, Tip k c #)

        Nil -> (# Nil, Nil #)

-- | \(\mathcal{O}(n)\).
--   Apply a function to every value in the tree and create two trees,
--   one out of 'Left' results and one out of 'Right' ones.
--
--   The 'Either' is evaluated to WHNF.
mapEitherWithKey :: (Word -> a -> Either b c) -> Patricia a -> Split b c
mapEitherWithKey f = \t ->
  case go t of
    (# !l, !r #) -> Split l r
  where
    go t =
      case t of
        Bin p l r ->
          let !(# !ll, !lr #) = go l
              !(# !rl, !rr #) = go r

          in (# rebin p ll rl, rebin p lr rr #)

        Tip k a ->
          case f k a of
            Left b  -> (# Tip k b, Nil #)
            Right c -> (# Nil, Tip k c #)

        Nil -> (# Nil, Nil #)



moduleLoc :: String
moduleLoc = "Patricia.Word.Strict"



-- | \(\mathcal{O}(\min(n,W))\).
--   Look up a value at the leftmost key in the tree.
lookupMin :: Patricia a -> Maybe a
lookupMin Nil = Nothing
lookupMin t   = let !(# a #) = unsafeLookupMin t
                in Just a

-- | \(\mathcal{O}(\min(n,W))\).
--   Look up a value at the leftmost key in the tree.
--
--   Throws 'MalformedTree' if the tree is empty.
unsafeLookupMin :: Patricia a -> (# a #)
unsafeLookupMin t =
  case t of
    Bin _ l _ -> unsafeLookupMin l
    Tip _ a   -> (# a #)
    Nil       -> throw $ MalformedTree moduleLoc "lookupMin"


-- | \(\mathcal{O}(\min(n,W))\).
--   Look up a value at the leftmost key in the tree.
lookupMinWithKey :: Patricia a -> Maybe (Lookup a)
lookupMinWithKey Nil = Nothing
lookupMinWithKey t   = Just $! unsafeLookupMinWithKey t

-- | \(\mathcal{O}(\min(n,W))\).
--   Look up a value at the leftmost key in the tree.
--
--   Throws 'MalformedTree' if the tree is empty.
unsafeLookupMinWithKey :: Patricia a -> Lookup a
unsafeLookupMinWithKey t =
  case t of
    Bin _ l _ -> unsafeLookupMinWithKey l
    Tip k a   -> Lookup k a
    Nil       -> throw $ MalformedTree moduleLoc "lookupMinWithKey"



-- | \(\mathcal{O}(\min(n,W))\).
--   Look up a value at the rightmost key in the tree.
lookupMax :: Patricia a -> Maybe a
lookupMax Nil = Nothing
lookupMax t   = let !(# a #) = unsafeLookupMax t
                in Just a

-- | \(\mathcal{O}(\min(n,W))\).
--   Look up a value at the rightmost key in the tree.
--
--   Throws 'MalformedTree' if the tree is empty.
unsafeLookupMax :: Patricia a -> (# a #)
unsafeLookupMax t =
  case t of
    Bin _ _ r -> unsafeLookupMax r
    Tip _ a   -> (# a #)
    Nil       -> throw $ MalformedTree moduleLoc "lookupMax"


-- | \(\mathcal{O}(\min(n,W))\).
--   Look up a value at the rightmost key in the tree.
lookupMaxWithKey :: Patricia a -> Maybe (Lookup a)
lookupMaxWithKey Nil = Nothing
lookupMaxWithKey t   = Just $! unsafeLookupMaxWithKey t

-- | \(\mathcal{O}(\min(n,W))\).
--   Look up a value at the rightmost key in the tree.
--
--   Throws 'MalformedTree' if the tree is empty.
unsafeLookupMaxWithKey :: Patricia a -> Lookup a
unsafeLookupMaxWithKey t =
  case t of
    Bin _ _ r -> unsafeLookupMaxWithKey r
    Tip k a   -> Lookup k a
    Nil       -> throw $ MalformedTree moduleLoc "lookupMaxWithKey"



-- | \(\mathcal{O}(\min(n,W))\).
--   Delete a value at the leftmost key in the tree.
deleteMin :: Patricia a -> Patricia a
deleteMin = go
  where
    go t =
      case t of
        Bin p l r -> rebinL p (go l) r
        _         -> Nil

-- | \(\mathcal{O}(\min(n,W))\).
--   Delete a value at the rightmost key in the tree.
deleteMax :: Patricia a -> Patricia a
deleteMax = go
  where
    go t =
      case t of
        Bin p l r -> rebinR p l (go r)
        _         -> Nil



-- | \(\mathcal{O}(\min(n,W))\).
--   Update a value at the leftmost key in the tree.
adjustMin :: (a -> a) -> Patricia a -> Patricia a
adjustMin f = go
  where
    go t =
      case t of
        Bin p l r -> Bin p (go l) r
        Tip k a   -> Tip k (f a)
        Nil       -> Nil

-- | \(\mathcal{O}(\min(n,W))\).
--   Update a value at the leftmost key in the tree.
--
--   New value is evaluated to WHNF.
adjustMin' :: (a -> a) -> Patricia a -> Patricia a
adjustMin' f = go
  where
    go t =
      case t of
        Bin p l r -> Bin p (go l) r
        Tip k a   -> Tip k $! f a
        Nil       -> Nil


-- | \(\mathcal{O}(\min(n,W))\).
--   Update a value at the leftmost key in the tree.
adjustMinWithKey :: (Word -> a -> a) -> Patricia a -> Patricia a
adjustMinWithKey f = go
  where
    go t =
      case t of
        Bin p l r -> Bin p (go l) r
        Tip k a   -> Tip k (f k a)
        Nil       -> Nil

-- | \(\mathcal{O}(\min(n,W))\).
--   Update a value at the leftmost key in the tree.
--
--   New value is evaluated to WHNF.
adjustMinWithKey' :: (Word -> a -> a) -> Patricia a -> Patricia a
adjustMinWithKey' f = go
  where
    go t =
      case t of
        Bin p l r -> Bin p (go l) r
        Tip k a   -> Tip k $! f k a
        Nil       -> Nil


-- | \(\mathcal{O}(\min(n,W))\).
--   Update a value at the rightmost key in the tree.
adjustMax :: (a -> a) -> Patricia a -> Patricia a
adjustMax f = go
  where
    go t =
      case t of
        Bin p l r -> Bin p l (go r)
        Tip k a   -> Tip k (f a)
        Nil       -> Nil

-- | \(\mathcal{O}(\min(n,W))\).
--   Update a value at the rightmost key in the tree.
--
--   New value is evaluated to WHNF.
adjustMax' :: (a -> a) -> Patricia a -> Patricia a
adjustMax' f = go
  where
    go t =
      case t of
        Bin p l r -> Bin p l (go r)
        Tip k a   -> Tip k $! f a
        Nil       -> Nil

-- | \(\mathcal{O}(\min(n,W))\).
--   Update a value at the rightmost key in the tree.
adjustMaxWithKey :: (Word -> a -> a) -> Patricia a -> Patricia a
adjustMaxWithKey f = go
  where
    go t =
      case t of
        Bin p l r -> Bin p l (go r)
        Tip k a   -> Tip k (f k a)
        Nil       -> Nil

-- | \(\mathcal{O}(\min(n,W))\).
--   Update a value at the rightmost key in the tree.
--
--   New value is evaluated to WHNF.
adjustMaxWithKey' :: (Word -> a -> a) -> Patricia a -> Patricia a
adjustMaxWithKey' f = go
  where
    go t =
      case t of
        Bin p l r -> Bin p l (go r)
        Tip k a   -> Tip k $! f k a
        Nil       -> Nil



-- | \(\mathcal{O}(\min(n,W))\).
--   Update or delete a value at the leftmost key in the tree.
--
--   The 'Maybe' is evaluated to WHNF.
updateMin :: (a -> Maybe a) -> Patricia a -> Patricia a
updateMin f = go
  where
    go t =
      case t of
        Bin p l r -> rebinL p (go l) r
        Tip k a   -> retip k (f a)
        Nil       -> Nil

-- | \(\mathcal{O}(\min(n,W))\).
--   Update or delete a value at the leftmost key in the tree.
--
--   The 'Maybe' is evaluated to WHNF.
updateMinWithKey :: (Word -> a -> Maybe a) -> Patricia a -> Patricia a
updateMinWithKey f = go
  where
    go t =
      case t of
        Bin p l r -> rebinL p (go l) r
        Tip k a   -> retip k (f k a)
        Nil       -> Nil


-- | \(\mathcal{O}(\min(n,W))\).
--   Update or delete a value at the rightmost key in the tree.
--
--   The 'Maybe' is evaluated to WHNF.
updateMax :: (a -> Maybe a) -> Patricia a -> Patricia a
updateMax f = go
  where
    go t =
      case t of
        Bin p l r -> rebinR p l (go r)
        Tip k a   -> retip k (f a)
        Nil       -> Nil

-- | \(\mathcal{O}(\min(n,W))\).
--   Update or delete a value at the rightmost key in the tree.
--
--   The 'Maybe' is evaluated to WHNF.
updateMaxWithKey :: (Word -> a -> Maybe a) -> Patricia a -> Patricia a
updateMaxWithKey f = go
  where
    go t =
      case t of
        Bin p l r -> rebinR p l (go r)
        Tip k a   -> retip k (f k a)
        Nil       -> Nil



-- | \(\mathcal{O}(\min(n,W))\).
--   Look up the leftmost value and return it alongside the tree without it.
minView :: Patricia a -> Maybe (ViewL a)
minView Nil = Nothing
minView t   = Just $! unsafeMinView t

-- | The leftmost value with its key and the rest of the tree.
data ViewL a = ViewL {-# UNPACK #-} !(Lookup a) !(Patricia a)
               deriving Show

-- | \(\mathcal{O}(\min(n,W))\).
--   Look up the leftmost value and return it alongside the tree without it.
--
--   Throws 'MalformedTree' if the tree is empty.
unsafeMinView :: Patricia a -> ViewL a
unsafeMinView t =
  case t of
    Bin p l r ->
      let !(ViewL a l0) = unsafeMinView l
      in ViewL a (rebinL p l0 r)

    Tip k a -> ViewL (Lookup k a) Nil

    Nil -> throw $ MalformedTree moduleLoc "minView"



-- | \(\mathcal{O}(\min(n,W))\).
--   Look up the rightmost value and return it alongside the tree without it.
maxView :: Patricia a -> Maybe (ViewR a)
maxView Nil = Nothing
maxView t   = Just $! unsafeMaxView t

-- | The rightmost value with its key and the rest of the tree.
data ViewR a = ViewR !(Patricia a) {-# UNPACK #-} !(Lookup a)
               deriving Show

-- | \(\mathcal{O}(\min(n,W))\).
--   Look up the rightmost value and return it alongside the tree without it.
--
--   Throws 'MalformedTree' if the tree is empty.
unsafeMaxView :: Patricia a -> ViewR a
unsafeMaxView t =
  case t of
    Bin p l r ->
      let !(ViewR r0 a) = unsafeMaxView r
      in ViewR (rebinR p l r0) a

    Tip k a -> ViewR Nil (Lookup k a)

    Nil -> throw $ MalformedTree moduleLoc "maxView"
