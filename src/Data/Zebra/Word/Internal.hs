{-# LANGUAGE BangPatterns
           , PatternSynonyms
           , ViewPatterns
           , UnboxedTuples
           , UnboxedSums #-}

module Data.Zebra.Word.Internal
  ( Color (..)
  , Zebra (Mono, ..)

  , Data.Zebra.Word.Internal.lookup
  , lookupL
  , findL
  , lookupR
  , findR

  , Range (..)

  , monoL
  , monoR
  , monoRange

  , unsafeMonoRange

  , size

  , sizeL
  , sizeR
  , sizeRange

  , unsafeSize
  , unsafeSizeL
  , unsafeSizeR
  , unsafeSizeRange

  , fillL
  , fillR
  , fillRange

  , unsafeFillL
  , unsafeFillRange

  , Data.Zebra.Word.Internal.foldl
  , foldlL
  , foldlR
  , foldlRange
  , unsafeFoldlRange

  , Data.Zebra.Word.Internal.foldr
  , foldrL
  , foldrR
  , foldrRange
  , unsafeFoldrRange

  , Data.Zebra.Word.Internal.foldl'
  , foldlL'
  , foldlR'
  , foldlRange'
  , unsafeFoldlRange'

  , Data.Zebra.Word.Internal.foldr'
  , foldrL'
  , foldrR'
  , foldrRange'
  , unsafeFoldrRange'

  , Data.Zebra.Word.Internal.complement

  , union
  , disjoint
  , intersection

  , difference
  , symmetricDifference

  , Data.Zebra.Word.Internal.compare
  ) where

import           Radix.Common (PartialOrdering (..), order)
import           Radix.Word.Common
import           Radix.Word.Foundation

import           Data.Bits
import           Numeric.Natural



-- | Space partition colors.
data Color = Black
           | White
             deriving (Show, Eq)

invert :: Color -> (# Color #)
invert Black = (# White #)
invert White = (# Black #)



-- | Fully-strict one-dimensional space partitioning tree.
data Zebra = Bin
               {-# UNPACK #-} !Prefix
               !Zebra                 -- ^ Masked bit is @0@.
               !Zebra                 -- ^ Masked bit is not @0@.

           | Bla
               -- | Invariant: can only be @0@ as the root of the tree.
               {-# UNPACK #-} !Key

           | Whi
               -- | Invariant: can only be @0@ as the root of the tree.
               {-# UNPACK #-} !Key

           | Nil                     -- ^ Invariant: unreachable state.
               {-# UNPACK #-} !Color

-- | Tree is represented as a list of closed intervals of all 'White' keys.
instance Show Zebra where
  showsPrec _ =
    let f (UnsafeRange kL kR) c z =
          case c of
            Black -> z
            White -> (kL, kR) : z

    in showList . Data.Zebra.Word.Internal.foldr f []

instance Eq Zebra where
  (==) = go
    where
      go l r =
        case l of
          Bin p xl xr ->
            case r of
              Bin q yl yr -> p == q && go xl yl && go xr yr
              _           -> False

          Bla kA ->
            case r of
              Bla kB -> kA == kB
              _      -> False

          Whi kA ->
            case r of
              Whi kB -> kA == kB
              _      -> False

          Nil _ -> False



-- | \(\mathcal{O}(1)\).
--   All keys are the same color.
pattern Mono :: Color -> Zebra
pattern Mono c <- ( ( \z -> case z of
                              Bla 0 -> Just Black
                              Whi 0 -> Just White
                              _     -> Nothing
                    )
                      -> Just c
                  )
  where
    Mono Black = Bla 0
    Mono White = Whi 0



{-# INLINE join #-}
-- | Knowing that the prefices of two non-'Nil' trees disagree, construct a 'Bin'.
join :: Prefix -> Zebra -> Prefix -> Zebra -> Zebra
join p0 t0 p1 t1 =
  let m = branchingBit p0 p1

      p = mask p0 m .|. m

  in if zeroBit p0 m
       then Bin p t0 t1
       else Bin p t1 t0

{-# INLINE rebin #-}
-- | Reconstruct a 'Bin' knowing that either of the sides may now be a 'Nil'.
rebin :: Prefix -> Zebra -> Zebra -> Zebra
rebin p l r =
  case l of
    Nil _ -> r
    _     ->
      case r of
        Nil _ -> l
        _     -> Bin p l r

{-# INLINE rebinL #-}
-- | Reconstruct a 'Bin' knowing that the left side may now be a 'Nil'.
rebinL :: Prefix -> Zebra -> Zebra -> Zebra
rebinL p l r =
  case l of
    Nil _ -> r
    _     -> Bin p l r


{-# INLINE rebinR #-}
-- | Reconstruct a 'Bin' knowing that the right side may now be a 'Nil'.
rebinR :: Prefix -> Zebra -> Zebra -> Zebra
rebinR p l r =
  case r of
    Nil _ -> l
    _     -> Bin p l r

{-# INLINE tip #-}
tip :: Key -> Color -> Zebra
tip k Black = Bla k
tip k White = Whi k



-- | \(\mathcal{O}(\min(n,W))\).
--   Check whether all keys smaller than or equal to the given key are of the same color.
monoL :: Word -> Zebra -> Maybe Color
monoL !w = go
  where
    go t =
      case t of
        Bin p l _ ->
          if w < p
            then if w >= lower p
                   then go l
                   else let !(# cR #) = colorL l
                            !(# cL #) = invert cR
                        in Just cL

            else Nothing

        Bla k       -> goTip Black k
        Whi k       -> goTip White k
        Nil _       -> Nothing

    goTip c k
      | k == 0    = Just c
      | w < k     = let !(# x #) = invert c
                    in Just x
      | otherwise = Nothing



-- | \(\mathcal{O}(\min(n,W))\).
--   Check whether all keys greater than or equal to the given key are of the same color.
monoR :: Word -> Zebra -> Maybe Color
monoR !w = go
  where
    go t =
      case t of
        Bin p _ r ->
          if w < p
            then Nothing
            else if w <= upper p
                   then go r
                   else let !(# cR #) = colorR r
                        in Just cR

        Bla k       -> goTip Black k
        Whi k       -> goTip White k
        Nil _       -> Nothing

    goTip c k
      | w >= k    = Just c
      | otherwise = Nothing



-- | \(\mathcal{O}(\min(n,W))\).
--   Check whether all keys in the range are of the same color.
monoRange :: Range -> Zebra -> Maybe Color
monoRange (UnsafeRange kL kR)
  | kR == maxBound = monoR kL
  | otherwise      = unsafeMonoRange kL (kR + 1)

-- | \(\mathcal{O}(\min(n,W))\).
--   Check whether all keys in the range are of the same color.
--
--    \(k_R\) __must__ be greater than \(k_L\).
unsafeMonoRange
  :: Word  -- ^ \(k_L\)
  -> Word  -- ^ \(k_R\)
  -> Zebra
  -> Maybe Color
unsafeMonoRange !wL !wR = go
  where
    !mM = branchingBit wL wR

    !pM = mask wL mM .|. mM

    go t =
      case t of
        Bin p l r ->
          case Prelude.compare p pM of
            EQ                 -> let !mcL = monoR wL l
                                      !mcR = monoL wR r

                                  in if mcL == mcR
                                       then mcL
                                       else Nothing

            LT | pM <= upper p -> go r
               | p >= lower pM -> if wL >= p
                                    then monoR wL r
                                    else Nothing

               | otherwise     -> let !(# cR #) = colorR r
                                  in Just cR

            GT | p <= upper pM -> if wR <= p
                                    then monoL wR l
                                    else Nothing

               | pM >= lower p -> go l
               | otherwise     -> let !(# cR #) = colorL l
                                      !(# cL #) = invert cR
                                  in Just cL

        Bla k       -> goTip Black k
        Whi k       -> goTip White k
        Nil _       -> Nothing

    goTip c k
      | wL >= k   = Just c
      | wR <= k   = let !(# x #) = invert c
                    in Just x
      | otherwise = Nothing



-- | \(\mathcal{O}(n)\).
--   Calculate the number of keys of the given color.
--   The returned number is guaranteed to be in the \([0, 2^W]\) interval.
size :: Color -> Zebra -> Natural
size !x t =
  case t of
    Bla 0 -> goZero Black
    Whi 0 -> goZero White
    _     -> fromIntegral $ unsafeSize x t
  where
    goZero c
      | x == c    = fromIntegral (maxBound :: Word) + 1
      | otherwise = 0

-- | \(\mathcal{O}(n)\).
--   Calculate the number of keys of the given color.
--
--   The tree __must not__ be 'Mono'.
unsafeSize :: Color -> Zebra -> Word
unsafeSize !x = size_ x 0 0

size_ :: Color -> Word -> Word -> Zebra -> Word
size_ !x = go
  where
    go !kL !kR t =
      case t of
        Bin p l r ->
          let !nL = go kL p l
              !nR = go p kR r

          in nL + nR

        Bla k -> goTip kL kR k Black
        Whi k -> goTip kL kR k White

        Nil _ -> 0

    goTip !kL !kR k c
      | x == c    = kR - k
      | otherwise = k - kL



-- | \(\mathcal{O}(\min(n,W) + n_L)\).
--   Calculate the number of keys of the given color that are smaller than
--   or equal to the given key.
--   The returned number is guaranteed to be in the \([0, 2^W]\) interval.
sizeL :: Color -> Word -> Zebra -> Natural
sizeL x w
  | w == maxBound = size x
  | otherwise     = fromIntegral . unsafeSizeL x (w + 1)

-- | \(\mathcal{O}(\min(n,W) + n_L)\).
--   Calculate the number of keys of the given color that are smaller than the given key.
--
--   The given key __must not__ be equal to @'Data.Bits.maxBound'@.
unsafeSizeL :: Color -> Word -> Zebra -> Word
unsafeSizeL x w = sizeL_ x 0 w

sizeL_ :: Color -> Word -> Word -> Zebra -> Word
sizeL_ !x !kL0 !w = go kL0
  where
    go !kL t =
      case t of
        Bin p l r ->
          if w < p
            then go kL l
            else
              let !nL = size_ x kL p l
                  !nR = go p r

              in nL + nR

        Bla k -> goTip kL k Black
        Whi k -> goTip kL k White

        Nil _ -> 0

    goTip !kL k c
      | x == c    = if w > k
                      then w - k
                      else 0

      | otherwise = let i | w > k     = k
                          | otherwise = w

                    in i - kL



-- | \(\mathcal{O}(\min(n,W) + n_R)\).
--   Calculate the number of keys of the given color that are greater than
--   or equal to the given key.
--   The returned number is guaranteed to be in the \([0, 2^W]\) interval.
sizeR :: Color -> Word -> Zebra -> Natural
sizeR x w
  | w == 0    = size x
  | otherwise = fromIntegral . unsafeSizeR x w

-- | \(\mathcal{O}(\min(n,W) + n_R)\).
--   Calculate the number of keys of the given color that are greater than
--   or equal to the given key.
--
--   The given key __must not__ be @0@.
unsafeSizeR :: Color -> Word -> Zebra -> Word
unsafeSizeR x w = sizeR_ x w 0

sizeR_ :: Color -> Word -> Word -> Zebra -> Word
sizeR_ !x !w = go
  where
    go !kR t =
      case t of
        Bin p l r ->
          if w < p
            then let !nL = go p l
                     !nR = size_ x p kR r

                 in nL + nR

            else go kR r

        Bla k -> goTip kR k Black
        Whi k -> goTip kR k White

        Nil _ -> 0

    goTip kR k c
      | x == c    = kR - if w > k
                           then w
                           else k

      | otherwise = if w < k
                      then k - w
                      else 0



-- | \(\mathcal{O}(\min(n,W) + n_I)\).
--   Calculate the number of keys of the given color in the range.
sizeRange :: Color -> Range -> Zebra -> Natural
sizeRange x (UnsafeRange kL kR)
  | kR == maxBound = sizeR x kL
  | otherwise      = fromIntegral . unsafeSizeRange x kL (kR + 1)

-- | \(\mathcal{O}(\min(n,W) + n_I)\).
--   Calculate the number of keys of the given color in the \([k_L, k_R)\) interval.
--
--   \(k_R\) __must__ be greater than \(k_L\).
unsafeSizeRange
  :: Color
  -> Word  -- ^ \(k_L\)
  -> Word  -- ^ \(k_R\)
  -> Zebra
  -> Word
unsafeSizeRange !x !wL !wR = go
  where
    !mM = branchingBit wL wR

    !pM = mask wL mM .|. mM

    go t =
      case t of
        Bin p l r ->
          case Prelude.compare p pM of
            EQ                 -> let !n = sizeR_ x wL p l
                                      !m = sizeL_ x p wR r

                                  in n + m

            LT | pM <= upper p -> go r
               | p >= lower pM -> if wL < p
                                    then let !n = sizeR_ x wL p l
                                             !m = size_ x p wR r

                                         in n + m

                                    else sizeR_ x wL wR r

               | otherwise     -> let !(# cR #) = colorR r
                                  in if cR == x
                                       then wR - wL
                                       else 0

            GT | p <= upper pM -> if wR >= p
                                    then let !n = size_ x wL p l
                                             !m = sizeL_ x p wR r

                                         in n + m

                                    else sizeL_ x wL wR l

               | pM >= lower p -> go l
               | otherwise     -> let !(# cR #) = colorL l
                                  in if cR == x
                                       then 0
                                       else wR - wL

        Bla k -> goTip k Black
        Whi k -> goTip k White

        Nil _ -> 0

    goTip k c
      | x == c    = if wR >= k
                      then wR - if wL > k
                                  then wL
                                  else k
                      else 0

      | otherwise = if wL <= k
                      then let i | wR > k    = k
                                 | otherwise = wR

                           in i - wL

                      else 0



-- | \(\mathcal{O}(n_R)\).
--   Fold left-to-right over the ranges.
foldl :: (a -> Range -> Color -> a) -> a -> Zebra -> a
foldl f = \z t ->
  case t of
    Bin _ l r -> let !(# w', x', z' #) = foldl_L 0 f z l
                 in foldl_R maxBound f w' x' z' r

    Bla k     -> tipM z k Black
    Whi k     -> tipM z k White
    Nil _     -> z
  where
    tipM z k c
      | k == 0    = let !r = UnsafeRange 0 maxBound
                    in f z r c

      | otherwise = let z' = let !k' = k - 1

                                 !(# x #) = invert c

                             in f z (UnsafeRange 0 k') x

                    in f z' (UnsafeRange k maxBound) c

foldl_L :: Word -> (a -> Range -> Color -> a) -> a -> Zebra -> (# Word, Color, a #)
foldl_L !wL f = go
  where
    go z t =
      case t of
        Bin _ l r -> let !(# w', x', z' #) = go z l
                     in foldl_M f w' x' z' r

        Bla k     -> goTip k Black
        Whi k     -> goTip k White
        Nil _     -> (# 0, Black, z #)
      where
        goTip k c = (# k, c, if k == 0
                               then z
                               else let !k' = k - 1

                                        !(# x #) = invert c

                                    in f z (UnsafeRange wL k') x
                     #)

foldl_R :: Word -> (a -> Range -> Color -> a) -> Word -> Color -> a -> Zebra -> a
foldl_R !wR f = go
  where
    go !w !x z t =
      case t of
        Bin _ l r -> let !(# w', x', z' #) = foldl_M f w x z l
                     in go w' x' z' r

        Bla k     -> goTip k Black
        Whi k     -> goTip k White
        Nil _     -> z
      where
        goTip k c = let z' = let !k' = k - 1
                             in f z (UnsafeRange w k') x

                        !r' = UnsafeRange k wR

                    in f z' r' c

foldl_M :: (a -> Range -> Color -> a) -> Word -> Color -> a -> Zebra -> (# Word, Color, a #)
foldl_M f = go
  where
    go w x z t =
      case t of
        Bin _ l r -> let !(# w', x', z' #) = go w x z l
                     in go w' x' z' r

        Bla k     -> goTip k Black
        Whi k     -> goTip k White
        Nil _     -> (# w, x, z #)
      where
        goTip k c = (# k, c, let !k' = k - 1
                             in f z (UnsafeRange w k') x
                     #)



-- | \(\mathcal{O}(n_R)\).
--   Fold left-to-right over the ranges of all the keys smaller than
--   or equal to the given one.
foldlL :: Word -> (a -> Range -> Color -> a) -> a -> Zebra -> a
foldlL = foldlL_ 0

foldlL_ :: Word -> Word -> (a -> Range -> Color -> a) -> a -> Zebra -> a
foldlL_ !wL !wR f = go
  where
    go z t =
      case t of
        Bin p l r ->
          if wR < p
            then go z l
            else let !(# w', x', z' #) = foldl_L wL f z l
                 in foldlL_R wR f w' x' z' r

        Bla k     -> tipM k Black
        Whi k     -> tipM k White
        Nil _     -> z
      where
        tipM k c
          | k == 0    = let !r = UnsafeRange wL wR
                        in f z r c

          | otherwise =
              let !(# x #) = invert c
              in if wR < k
                   then f z (UnsafeRange wL wR) x
                   else let z' = let !k' = k - 1
                                 in f z (UnsafeRange wL k') x

                        in f z' (UnsafeRange k wR) c

foldlL_R :: Word -> (a -> Range -> Color -> a) -> Word -> Color -> a -> Zebra -> a
foldlL_R !wR f = go
  where
    go !w !x z t =
      case t of
        Bin p l r ->
          if wR < p
            then go w x z l
            else let !(# w', x', z' #) = foldl_M f w x z l
                 in go w' x' z' r

        Bla k     -> goTip k Black
        Whi k     -> goTip k White
        Nil _     -> z
      where
        goTip k c
          | wR < k    = let !r = UnsafeRange w wR
                        in f z r x

          | otherwise = let z' = let !k' = k - 1
                                 in f z (UnsafeRange w k') x

                        in f z' (UnsafeRange k wR) c



-- | \(\mathcal{O}(n_R)\).
--   Fold left-to-right over the ranges of all the keys greater than
--   or equal to the given one.
foldlR :: Word -> (a -> Range -> Color -> a) -> a -> Zebra -> a
foldlR wL = foldlR_ wL maxBound

foldlR_ :: Word -> Word -> (a -> Range -> Color -> a) -> a -> Zebra -> a
foldlR_ !wL !wR f = go
  where
    go z t =
      case t of
        Bin p l r ->
          if wL < p
            then let !(# w', x', z' #) = foldlR_L wL f z l
                 in foldl_R wR f w' x' z' r

            else go z r

        Bla k     -> tipM k Black
        Whi k     -> tipM k White
        Nil _     -> z
      where
        tipM k c
          | wL >= k   = f z (UnsafeRange wL wR) c
          | otherwise = let !k' = k - 1
                            !(# x #) = invert c

                            z' = f z (UnsafeRange wL k') x

                        in f z' (UnsafeRange k wR) c

foldlR_L :: Word -> (a -> Range -> Color -> a) -> a -> Zebra -> (# Word, Color, a #)
foldlR_L !wL f = go
  where
    go z t =
      case t of
        Bin p l r ->
          if wL < p
            then let !(# w', x', z' #) = go z l
                 in foldl_M f w' x' z' r

            else go z r

        Bla k     -> goTip k Black
        Whi k     -> goTip k White
        Nil _     -> (# 0, Black, z #)
      where
        goTip k c
          | wL >= k   = (# wL, c, z #)

          | otherwise = let !k' = k - 1
                            !(# x #) = invert c

                        in (# k, c, f z (UnsafeRange wL k') x #)



-- | \(\mathcal{O}(\min(n,W) + n_{I_R})\).
--   Fold left-to-right over the ranges of all the keys in the given range.
foldlRange :: Range -> (a -> Range -> Color -> a) -> a -> Zebra -> a
foldlRange (UnsafeRange wL wR) f z
  | wL == wR  = \t -> let !c = Data.Zebra.Word.Internal.lookup wL t
                      in f z (UnsafeRange wL wR) c

  | otherwise = unsafeFoldlRange wL wR f z

-- | \(\mathcal{O}(n)\).
--   Fold left-to-right over the ranges of all the keys
--   in the \([k_L, k_R)\) interval.
--
--   \(k_R\) __must__ be greater than \(k_L\).
unsafeFoldlRange
  :: Word                       -- ^ \(k_L\)
  -> Word                       -- ^ \(k_R\)
  -> (a -> Range -> Color -> a)
  -> a
  -> Zebra
  -> a
unsafeFoldlRange !wL !wR f = go
  where
    !mM = branchingBit wL wR

    !pM = mask wL mM .|. mM

    go z t =
      case t of
        Bin p l r ->
          case Prelude.compare p pM of
            EQ                 -> let !(# w', x', z' #) = foldlR_L wL f z l
                                  in foldlL_R wR f w' x' z' r

            LT | pM <= upper p -> go z r
               | p >= lower pM -> if wL < p
                                    then let !(# w', x', z' #) = foldlR_L wL f z l
                                         in foldl_R wR f w' x' z' r

                                    else foldlR_ wL wR f z r

               | otherwise     -> let !(# cR #) = colorR r
                                  in f z (UnsafeRange wL wR) cR

            GT | p <= upper pM -> if wR >= p
                                    then let !(# w', x', z' #) = foldl_L wL f z l
                                         in foldlL_R wR f w' x' z' r

                                    else foldlL_ wL wR f z l

               | pM >= lower p -> go z l
               | otherwise     -> let !(# cR #) = colorL l
                                      !(# cL #) = invert cR

                                  in f z (UnsafeRange wL wR) cL

        Bla k     -> tipM k Black
        Whi k     -> tipM k White
        Nil _     -> z
      where
        tipM k c
          | wL >= k   = f z (UnsafeRange wL wR) c
          | otherwise =
              let !(# x #) = invert c
              in if wR < k
                   then f z (UnsafeRange wL wR) x
                   else let !k' = k - 1

                            z' = f z (UnsafeRange wL k') x

                        in f z' (UnsafeRange k wR) c



-- | \(\mathcal{O}(n_L)\).
--   Fold right-to-left over the ranges.
foldr :: (Range -> Color -> a -> a) -> a -> Zebra -> a
foldr f = \z t ->
  case t of
    Bin _ l r -> let !(# w', x', z' #) = foldr_R maxBound f z r
                 in foldr_L 0 f w' x' z' l

    Bla k     -> goTip z k Black
    Whi k     -> goTip z k White
    Nil _     -> z
  where
    goTip z k c
      | k == 0    = f (UnsafeRange 0 maxBound) c z

      | otherwise = let !k' = k - 1

                        !(# x #) = invert c

                    in f (UnsafeRange 0 k') x $ f (UnsafeRange k maxBound) c z

foldr_R :: Word -> (Range -> Color -> a -> a) -> a -> Zebra -> (# Word, Color, a #)
foldr_R !wR f = go
  where
    go z t =
      case t of
        Bin _ l r -> let !(# w', x', z' #) = go z r
                     in foldr_M f w' x' z' l

        Bla k     -> goTip k Black
        Whi k     -> goTip k White
        Nil _     -> (# 0, Black, z #)
      where
        goTip k c = let !k' = k - 1
                    in (# k', c, f (UnsafeRange k wR) c z #)

foldr_L :: Word -> (Range -> Color -> a -> a) -> Word -> Color -> a -> Zebra -> a
foldr_L !wL f = go
  where
    go !w !x z t =
      case t of
        Bin _ l r -> let !(# w', x', z' #) = foldr_M f w x z r
                     in go w' x' z' l

        Bla k     -> goTip k Black
        Whi k     -> goTip k White
        Nil _     -> z
      where
        goTip k c
          | k == 0    = f (UnsafeRange wL w) c z

          | otherwise = let !k' = k - 1
                        in f (UnsafeRange wL k') x $ f (UnsafeRange k w) c z

foldr_M
  :: (Range -> Color -> a -> a) -> Word -> Color -> a -> Zebra -> (# Word, Color, a #)
foldr_M f = go
  where
    go w x z t =
      case t of
        Bin _ l r -> let !(# w', x', z' #) = go w x z r
                     in go w' x' z' l

        Bla k     -> goTip k Black
        Whi k     -> goTip k White
        Nil _     -> (# w, x, z #)
      where
        goTip k c = let !k' = k - 1
                    in (# k', c, f (UnsafeRange k w) c z #)



-- | \(\mathcal{O}(n_L)\).
--   Fold right-to-left over the ranges of all the keys greater than
--   or equal to the given one.
foldrR :: Word -> (Range -> Color -> a -> a) -> a -> Zebra -> a
foldrR wL = foldrR_ wL maxBound

foldrR_ :: Word -> Word -> (Range -> Color -> a -> a) -> a -> Zebra -> a
foldrR_ !wL !wR f = go
  where
    go z t =
      case t of
        Bin p l r ->
          if wL < p
            then let !(# w', x', z' #) = foldr_R wR f z r
                 in foldrR_L wL f w' x' z' l

            else go z r

        Bla k     -> goTip k Black
        Whi k     -> goTip k White
        Nil _     -> z
      where
        goTip k c
          | k == 0    = f (UnsafeRange wL wR) c z

          | wL < k    = let !k' = k - 1

                            !(# x #) = invert c

                        in f (UnsafeRange wL k') x $ f (UnsafeRange k wR) c z

          | otherwise = f (UnsafeRange wL wR) c z

foldrR_L :: Word -> (Range -> Color -> a -> a) -> Word -> Color -> a -> Zebra -> a
foldrR_L !wL f = go
  where
    go !w !x z t =
      case t of
        Bin p l r ->
          if wL < p
            then let !(# w', x', z' #) = foldr_M f w x z r
                 in go w' x' z' l

            else go w x z r

        Bla k     -> goTip k Black
        Whi k     -> goTip k White
        Nil _     -> z
      where
        goTip k c
          | wL < k    = let !k' = k - 1
                        in f (UnsafeRange wL k') x $ f (UnsafeRange k w) c z

          | otherwise = f (UnsafeRange wL w) c z



-- | \(\mathcal{O}(n_L)\).
--   Fold right-to-left over the ranges of all the keys smaller than
--   or equal to the given one.
foldrL :: Word -> (Range -> Color -> a -> a) -> a -> Zebra -> a
foldrL = foldrL_ 0

foldrL_ :: Word -> Word -> (Range -> Color -> a -> a) -> a -> Zebra -> a
foldrL_ !wL !wR f = go
  where
    go z t =
      case t of
        Bin p l r ->
          if wR < p
            then go z l
            else let !(# w', x', z' #) = foldrL_R wR f z r
                 in foldr_L wL f w' x' z' l

        Bla k     -> goTip k Black
        Whi k     -> goTip k White
        Nil _     -> z
      where
        goTip k c
          | k == 0    = f (UnsafeRange wL wR) c z

          | wR >= k   = let !k' = k - 1

                            !(# x #) = invert c

                        in f (UnsafeRange wL k') x $ f (UnsafeRange k wR) c z

          | otherwise = let !(# x #) = invert c
                        in f (UnsafeRange wL wR) x z

foldrL_R :: Word -> (Range -> Color -> a -> a) -> a -> Zebra -> (# Word, Color, a #)
foldrL_R !wR f = go
  where
    go z t =
      case t of
        Bin p l r ->
          if wR < p
            then go z l
            else let !(# w', x', z' #) = go z r
                 in foldr_M f w' x' z' l

        Bla k     -> goTip k Black
        Whi k     -> goTip k White
        Nil _     -> (# 0, Black, z #)
      where
        goTip k c
          | wR >= k   = let !k' = k - 1
                        in (# k', c, f (UnsafeRange k wR) c z #)

          | otherwise = (# wR, c, z #)



-- | \(\mathcal{O}(\min(n,W) + n_{I_L})\).
--   Fold right-to-left over the ranges of all the keys in the given range.
foldrRange :: Range -> (Range -> Color -> a -> a) -> a -> Zebra -> a
foldrRange (UnsafeRange wL wR) f z
  | wL == wR  = \t -> let !c = Data.Zebra.Word.Internal.lookup wL t
                      in f (UnsafeRange wL wR) c z

  | otherwise = unsafeFoldrRange wL wR f z

-- | \(\mathcal{O}(\min(n,W) + n_{I_L})\).
--   Fold right-to-left over the ranges of all the keys
--   in the \([k_L, k_R)\) interval.
--
--   \(k_R\) __must__ be greater than \(k_L\).
unsafeFoldrRange
  :: Word                       -- ^ \(k_L\)
  -> Word                       -- ^ \(k_R\)
  -> (Range -> Color -> a -> a)
  -> a
  -> Zebra
  -> a
unsafeFoldrRange !wL !wR f = go
  where
    !mM = branchingBit wL wR

    !pM = mask wL mM .|. mM

    go z t =
      case t of
        Bin p l r ->
          case Prelude.compare p pM of
            EQ                 -> let !(# w', x', z' #) = foldrL_R wR f z r
                                  in foldrR_L wL f w' x' z' l

            LT | pM <= upper p -> go z r
               | p >= lower pM -> if wL < p
                                    then let !(# w', x', z' #) = foldrL_R wR f z r
                                         in foldr_L wL f w' x' z' l

                                    else foldrR_ wL wR f z r

               | otherwise     -> let !(# cR #) = colorR r
                                  in f (UnsafeRange wL wR) cR z

            GT | p <= upper pM -> if wR >= p
                                    then let !(# w', x', z' #) = foldr_R wR f z r
                                         in foldrR_L wL f w' x' z' l

                                    else foldrL_ wL wR f z l

               | pM >= lower p -> go z l

               | otherwise     -> let !(# cR #) = colorL l
                                      !(# cL #) = invert cR

                                  in f (UnsafeRange wL wR) cL z

        Bla k     -> tipM k Black
        Whi k     -> tipM k White
        Nil _     -> z
      where
        tipM k c
          | wL >= k   = f (UnsafeRange wL wR) c z
          | otherwise =
              let !(# x #) = invert c
              in if wR < k
                   then f (UnsafeRange wL wR) x z
                   else let !k' = k - 1
                        in f (UnsafeRange wL k') x $ f (UnsafeRange k wR) c z



-- | \(\mathcal{O}(n)\).
--   Fold left-to-right over the ranges with a strict accumulator.
foldl' :: (a -> Range -> Color -> a) -> a -> Zebra -> a
foldl' f = \ !z t ->
  case t of
    Bin _ l r -> let !(# w', x', z' #) = foldl'_L 0 f z l
                 in foldl'_R maxBound f w' x' z' r

    Bla k     -> goTip z k Black
    Whi k     -> goTip z k White
    Nil _     -> z
  where
    goTip z k c
      | k == 0    = f z (UnsafeRange 0 maxBound) c

      | otherwise = let !z' = let !k' = k - 1

                                  !(# x #) = invert c

                              in f z (UnsafeRange 0 k') x

                    in f z' (UnsafeRange k maxBound) c

foldl'_L :: Word -> (a -> Range -> Color -> a) -> a -> Zebra -> (# Word, Color, a #)
foldl'_L !wL f = go
  where
    go !z t =
      case t of
        Bin _ l r -> let !(# w', x', z' #) = go z l
                     in foldl'_M f w' x' z' r

        Bla k     -> goTip k Black
        Whi k     -> goTip k White
        Nil _     -> (# 0, Black, z #)
      where
        goTip k c = let !k' = k - 1

                        !(# x #) = invert c

                    in (# k, c, if k == 0
                                  then z
                                  else f z (UnsafeRange wL k') x #)

foldl'_R :: Word -> (a -> Range -> Color -> a) -> Word -> Color -> a -> Zebra -> a
foldl'_R !wR f = go
  where
    go !w !x !z t =
      case t of
        Bin _ l r -> let !(# w', x', z' #) = foldl'_M f w x z l
                     in go w' x' z' r

        Bla k     -> goTip k Black
        Whi k     -> goTip k White
        Nil _     -> z
      where
        goTip k c = let !z' = f z (UnsafeRange w (k - 1)) x
                    in f z' (UnsafeRange k wR) c

foldl'_M
  :: (a -> Range -> Color -> a) -> Word -> Color -> a -> Zebra -> (# Word, Color, a #)
foldl'_M f = go
  where
    go w x !z t =
      case t of
        Bin _ l r -> let !(# w', x', z' #) = go w x z l
                     in go w' x' z' r

        Bla k     -> goTip k Black
        Whi k     -> goTip k White
        Nil _     -> (# w, x, z #)
      where
        goTip k c = (# k, c, f z (UnsafeRange w (k - 1)) x #)



-- | \(\mathcal{O}(n)\).
--   Fold left-to-right over the ranges of all the keys smaller than
--   or equal to the given one.
foldlL' :: Word -> (a -> Range -> Color -> a) -> a -> Zebra -> a
foldlL' = foldlL'_ 0

foldlL'_ :: Word -> Word -> (a -> Range -> Color -> a) -> a -> Zebra -> a
foldlL'_ !wL !wR f = go
  where
    go !z t =
      case t of
        Bin p l r ->
          if wR < p
            then go z l
            else let !(# w', x', z' #) = foldl'_L wL f z l
                 in foldlL'_R wR f w' x' z' r

        Bla k     -> tipM k Black
        Whi k     -> tipM k White
        Nil _     -> z
      where
        tipM k c
          | k == 0    = f z (UnsafeRange wL wR) c

          | wR < k    = let !(# x #) = invert c
                        in f z (UnsafeRange wL wR) x

          | otherwise = let !z' = let !k' = k - 1

                                      !(# x #) = invert c

                                      in f z (UnsafeRange wL k') x

                        in f z' (UnsafeRange k wR) c

foldlL'_R :: Word -> (a -> Range -> Color -> a) -> Word -> Color -> a -> Zebra -> a
foldlL'_R !wR f = go
  where
    go !w !x !z t =
      case t of
        Bin p l r ->
          if wR < p
            then go w x z l
            else let !(# w', x', z' #) = foldl'_M f w x z l
                 in go w' x' z' r

        Bla k     -> goTip k Black
        Whi k     -> goTip k White
        Nil _     -> z
      where
        goTip k c
          | wR < k    = f z (UnsafeRange w wR) x
          | otherwise = let z' = f z (UnsafeRange w (k - 1)) x
                        in f z' (UnsafeRange k wR) c



-- | \(\mathcal{O}(n)\).
--   Fold left-to-right over the ranges of all the keys greater than
--   or equal to the given one.
foldlR' :: Word -> (a -> Range -> Color -> a) -> a -> Zebra -> a
foldlR' wL = foldlR'_ wL maxBound

foldlR'_ :: Word -> Word -> (a -> Range -> Color -> a) -> a -> Zebra -> a
foldlR'_ !wL !wR f = go
  where
    go !z t =
      case t of
        Bin p l r ->
          if wL < p
            then let !(# w', x', z' #) = foldlR'_L wL f z l
                 in foldl'_R wR f w' x' z' r

            else go z r

        Bla k     -> tipM k Black
        Whi k     -> tipM k White
        Nil _     -> z
      where
        tipM k c
          | wL >= k   = f z (UnsafeRange wL wR) c
          | otherwise = let !z' = let !k' = k - 1

                                      !(# x #) = invert c

                                      in f z (UnsafeRange wL k') x

                        in f z' (UnsafeRange k wR) c

foldlR'_L :: Word -> (a -> Range -> Color -> a) -> a -> Zebra -> (# Word, Color, a #)
foldlR'_L !wL f = go
  where
    go !z t =
      case t of
        Bin p l r ->
          if wL < p
            then let !(# w', x', z' #) = go z l
                 in foldl'_M f w' x' z' r

            else go z r

        Bla k     -> goTip k Black
        Whi k     -> goTip k White
        Nil _     -> (# 0, Black, z #)
      where
        goTip k c
          | wL >= k   = (# wL, c, z #)
          | otherwise = let !k' = k - 1

                            !(# x #) = invert c

                        in (# k, c, f z (UnsafeRange wL k') x #)



-- | \(\mathcal{O}(\min(n,W) + n_I)\).
--   Fold left-to-right over the ranges of all the keys in the given range.
foldlRange' :: Range -> (a -> Range -> Color -> a) -> a -> Zebra -> a
foldlRange' (UnsafeRange wL wR) f z
  | wL == wR  = \t -> let !c = Data.Zebra.Word.Internal.lookup wL t
                      in f z (UnsafeRange wL wR) c

  | otherwise = unsafeFoldlRange' wL wR f z

-- | \(\mathcal{O}(n)\).
--   Fold left-to-right over the ranges of all the keys
--   in the \([k_L, k_R)\) interval.
--
--   \(k_R\) __must__ be greater than \(k_L\).
unsafeFoldlRange'
  :: Word                       -- ^ \(k_L\)
  -> Word                       -- ^ \(k_R\)
  -> (a -> Range -> Color -> a)
  -> a
  -> Zebra
  -> a
unsafeFoldlRange' !wL !wR f = go
  where
    !mM = branchingBit wL wR

    !pM = mask wL mM .|. mM

    go z t =
      case t of
        Bin p l r ->
          case Prelude.compare p pM of
            EQ                 -> let !(# w', x', z' #) = foldlR'_L wL f z l
                                  in foldlL'_R wR f w' x' z' r

            LT | pM <= upper p -> go z r
               | p >= lower pM -> if wL < p
                                    then let !(# w', x', z' #) = foldlR'_L wL f z l
                                         in foldl'_R wR f w' x' z' r

                                    else foldlR'_ wL wR f z r

               | otherwise     -> let !(# cR #) = colorR r
                                  in f z (UnsafeRange wL wR) cR

            GT | p <= upper pM -> if wR >= p
                                    then let !(# w', x', z' #) = foldl'_L wL f z l
                                         in foldlL'_R wR f w' x' z' r

                                    else foldlL'_ wL wR f z l

               | pM >= lower p -> go z l
               | otherwise     -> let !(# cR #) = colorL l
                                      !(# cL #) = invert cR

                                  in f z (UnsafeRange wL wR) cL

        Bla k     -> tipM k Black
        Whi k     -> tipM k White
        Nil _     -> z
      where
        tipM k c
          | wL >= k   = f z (UnsafeRange wL wR) c
          | otherwise =
              let !(# x #) = invert c
              in if wR < k
                   then f z (UnsafeRange wL wR) x
                   else let !k' = k - 1

                            z' = f z (UnsafeRange wL k') x

                        in f z' (UnsafeRange k wR) c



-- | \(\mathcal{O}(n)\).
--   Fold right-to-left over the ranges.
foldr' :: (Range -> Color -> a -> a) -> a -> Zebra -> a
foldr' f = \ !z t ->
  case t of
    Bin _ l r -> let !(# w', x', z' #) = foldr'_R maxBound f z r
                 in foldr'_L 0 f w' x' z' l

    Bla k     -> goTip z k Black
    Whi k     -> goTip z k White
    Nil _     -> z
  where
    goTip z k c
      | k == 0    = f (UnsafeRange 0 maxBound) c z

      | otherwise = let !k' = k - 1

                        !(# x #) = invert c

                    in f (UnsafeRange 0 k') x $! f (UnsafeRange k maxBound) c z

foldr'_R :: Word -> (Range -> Color -> a -> a) -> a -> Zebra -> (# Word, Color, a #)
foldr'_R !wR f = go
  where
    go !z t =
      case t of
        Bin _ l r -> let !(# w', x', z' #) = go z r
                     in foldr'_M f w' x' z' l

        Bla k     -> goTip k Black
        Whi k     -> goTip k White
        Nil _     -> (# 0, Black, z #)
      where
        goTip k c = let !k' = k - 1
                    in (# k', c, f (UnsafeRange k wR) c z #)

foldr'_L :: Word -> (Range -> Color -> a -> a) -> Word -> Color -> a -> Zebra -> a
foldr'_L !wL f = go
  where
    go !w !x !z t =
      case t of
        Bin _ l r -> let !(# w', x', z' #) = foldr'_M f w x z r
                     in go w' x' z' l

        Bla k     -> goTip k Black
        Whi k     -> goTip k White
        Nil _     -> z
      where
        goTip k c
          | k == 0    = f (UnsafeRange wL w) c z

          | otherwise = let !k' = k - 1
                        in f (UnsafeRange wL k') x $! f (UnsafeRange k w) c z

foldr'_M
  :: (Range -> Color -> a -> a) -> Word -> Color -> a -> Zebra -> (# Word, Color, a #)
foldr'_M f = go
  where
    go w x !z t =
      case t of
        Bin _ l r -> let !(# w', x', z' #) = go w x z r
                     in go w' x' z' l

        Bla k     -> goTip k Black
        Whi k     -> goTip k White
        Nil _     -> (# w, x, z #)
      where
        goTip k c = let !k' = k - 1
                    in (# k', c, f (UnsafeRange k w) c z #)



-- | \(\mathcal{O}(n)\).
--   Fold right-to-left over the ranges of all the keys greater than
--   or equal to the given one.
foldrR' :: Word -> (Range -> Color -> a -> a) -> a -> Zebra -> a
foldrR' wL = foldrR'_ wL maxBound

foldrR'_ :: Word -> Word -> (Range -> Color -> a -> a) -> a -> Zebra -> a
foldrR'_ !wL !wR f = go
  where
    go !z t =
      case t of
        Bin p l r ->
          if wL < p
            then let !(# w', x', z' #) = foldr'_R wR f z r
                 in foldrR'_L wL f w' x' z' l

            else go z r

        Bla k     -> goTip k Black
        Whi k     -> goTip k White
        Nil _     -> z
      where
        goTip k c
          | k == 0    = f (UnsafeRange wL wR) c z

          | wL < k    = let !k' = k - 1

                            !(# x #) = invert c

                        in f (UnsafeRange wL k') x $! f (UnsafeRange k wR) c z

          | otherwise = f (UnsafeRange wL wR) c z

foldrR'_L :: Word -> (Range -> Color -> a -> a) -> Word -> Color -> a -> Zebra -> a
foldrR'_L !wL f = go
  where
    go !w !x !z t =
      case t of
        Bin p l r ->
          if wL < p
            then let !(# w', x', z' #) = foldr'_M f w x z r
                 in go w' x' z' l

            else go w x z r

        Bla k     -> goTip k Black
        Whi k     -> goTip k White
        Nil _     -> z
      where
        goTip k c
          | wL < k    = let !k' = k - 1
                        in f (UnsafeRange wL k') x $! f (UnsafeRange k w) c z

          | otherwise = f (UnsafeRange wL w) c z



-- | \(\mathcal{O}(n)\).
--   Fold right-to-left over the ranges of all the keys smaller than
--   or equal to the given one.
foldrL' :: Word -> (Range -> Color -> a -> a) -> a -> Zebra -> a
foldrL' = foldrL'_ 0

foldrL'_ :: Word -> Word -> (Range -> Color -> a -> a) -> a -> Zebra -> a
foldrL'_ !wL !wR f = go
  where
    go !z t =
      case t of
        Bin p l r ->
          if wR < p
            then go z l
            else let !(# w', x', z' #) = foldrL'_R wR f z r
                 in foldr'_L wL f w' x' z' l

        Bla k     -> goTip k Black
        Whi k     -> goTip k White
        Nil _     -> z
      where
        goTip k c
          | k == 0    = f (UnsafeRange wL wR) c z

          | wR >= k   = let !k' = k - 1

                            !(# x #) = invert c

                        in f (UnsafeRange wL k') x $! f (UnsafeRange k wR) c z

          | otherwise = let !(# x #) = invert c
                        in f (UnsafeRange wL wR) x z

foldrL'_R :: Word -> (Range -> Color -> a -> a) -> a -> Zebra -> (# Word, Color, a #)
foldrL'_R !wR f = go
  where
    go !z t =
      case t of
        Bin p l r ->
          if wR < p
            then go z l
            else let !(# w', x', z' #) = go z r
                 in foldr'_M f w' x' z' l

        Bla k     -> goTip k Black
        Whi k     -> goTip k White
        Nil _     -> (# 0, Black, z #)
      where
        goTip k c
          | wR >= k   = let !k' = k - 1
                        in (# k', c, f (UnsafeRange k wR) c z #)

          | otherwise = (# wR, c, z #)



-- | \(\mathcal{O}(\min(n,W) + n_I)\).
--   Fold right-to-left with a strict accumulator over the ranges of all the keys
--   in the given range.
foldrRange' :: Range -> (Range -> Color -> a -> a) -> a -> Zebra -> a
foldrRange' (UnsafeRange wL wR) f !z
  | wL == wR  = \t -> let !c = Data.Zebra.Word.Internal.lookup wL t
                      in f (UnsafeRange wL wR) c z

  | otherwise = unsafeFoldrRange' wL wR f z

-- | \(\mathcal{O}(\min(n,W) + n_I)\).
--   Fold right-to-left with a strict accumulator over the ranges of all the keys
--   in the \([k_L, k_R)\) interval.
--
--   \(k_R\) __must__ be greater than \(k_L\).
unsafeFoldrRange'
  :: Word                       -- ^ \(k_L\)
  -> Word                       -- ^ \(k_R\)
  -> (Range -> Color -> a -> a)
  -> a
  -> Zebra
  -> a
unsafeFoldrRange' !wL !wR f = go
  where
    !mM = branchingBit wL wR

    !pM = mask wL mM .|. mM

    go !z t =
      case t of
        Bin p l r ->
          case Prelude.compare p pM of
            EQ                 -> let !(# w', x', z' #) = foldrL'_R wR f z r
                                  in foldrR'_L wL f w' x' z' l

            LT | pM <= upper p -> go z r
               | p >= lower pM -> if wL < p
                                    then let !(# w', x', z' #) = foldrL'_R wR f z r
                                         in foldr'_L wL f w' x' z' l

                                    else foldrR'_ wL wR f z r

               | otherwise     -> let !(# cR #) = colorR r
                                  in f (UnsafeRange wL wR) cR z

            GT | p <= upper pM -> if wR >= p
                                    then let !(# w', x', z' #) = foldr'_R wR f z r
                                         in foldrR'_L wL f w' x' z' l

                                    else foldrL'_ wL wR f z l

               | pM >= lower p -> go z l

               | otherwise     -> let !(# cR #) = colorL l
                                      !(# cL #) = invert cR

                                  in f (UnsafeRange wL wR) cL z

        Bla k     -> tipM k Black
        Whi k     -> tipM k White
        Nil _     -> z
      where
        tipM k c
          | wL >= k   = f (UnsafeRange wL wR) c z
          | otherwise =
              let !(# x #) = invert c
              in if wR < k
                   then f (UnsafeRange wL wR) x z
                   else let !k' = k - 1
                        in f (UnsafeRange wL k') x $! f (UnsafeRange k wR) c z




-- | \(\mathcal{O}(\min(n,W))\).
--   Look up the color of the key.
lookup :: Word -> Zebra -> Color
lookup !w = go
  where
    go t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then go l
                   else let !(# cR #) = colorL l
                            !(# cL #) = invert cR
                        in cL

            else if w <= upper p
                   then go r
                   else let !(# cR #) = colorR r
                        in cR

        Bla k -> goTip k Black
        Whi k -> goTip k White

        Nil _ -> Black

    goTip k c
      | w < k     = let !(# cL #) = invert c
                    in cL
      | otherwise = c



-- | \(\mathcal{O}(\min(n,W))\).
--   Look up the key of the given color that is smaller than or equal to the given key.
lookupL :: Color -> Word -> Zebra -> Maybe Word
lookupL !x !w = go (Nil Black)
  where
    go !v t =
      case t of
        Bin p l r
          | w < p     -> go v l
          | otherwise -> go l r

        Bla k -> goTip Black k v
        Whi k -> goTip White k v

        Nil _ -> Nothing

    goTip c k v =
      case w >= k of
        True
          | k == 0    -> if c == x
                           then Just w
                           else Nothing

          | otherwise -> Just $! if c == x
                                   then w
                                   else k - 1
        False
          | c == x    -> case v of
                           Nil _ -> Nothing
                           _     -> let !(# kL #) = keyR v
                                    in Just $! kL - 1

          | otherwise -> Just w



-- | \(\mathcal{O}(\min(n,W))\).
--   Look up the key of the given color that is smaller than or equal to the given key,
--   falling back to the default value if no such key exists.
findL
  :: Word -- ^ Default value
  -> Color
  -> Word -- ^ Key
  -> Zebra
  -> Word
findL d !x !w = go (Nil Black)
  where
    go !v t =
      case t of
        Bin p l r
          | w < p     -> go v l
          | otherwise -> go l r

        Bla k -> goTip Black k v
        Whi k -> goTip White k v

        Nil _ -> d

    goTip c k v =
      case w >= k of
        True
          | k == 0    -> if c == x
                           then w
                           else d

          | c == x    -> w
          | otherwise -> k - 1

        False
          | c == x    -> case v of
                           Nil _ -> d
                           _     -> let !(# kL #) = keyR v
                                    in kL - 1
          | otherwise -> w



-- | \(\mathcal{O}(\min(n,W))\).
--   Look up the key of the given color that is greater than or equal to the given key.
lookupR :: Color -> Word -> Zebra -> Maybe Word
lookupR !x !w = go (Nil Black)
  where
    go !v t =
      case t of
        Bin p l r
          | w < p     -> go r l
          | otherwise -> go v r

        Bla k -> goTip Black k v
        Whi k -> goTip White k v

        Nil _ -> Nothing

    goTip c k v =
      case w < k of
        True -> Just $! if c == x
                          then k
                          else w
        False
          | c == x    -> Just w
          | otherwise -> case v of
                           Nil _ -> Nothing
                           _     -> let !(# kR #) = keyL v
                                    in Just kR



-- | \(\mathcal{O}(\min(n,W))\).
--   Look up the key of the given color that is greater than or equal to the given key,
--   falling back to the default value if no such key exists.
findR
  :: Word  -- ^ Default value
  -> Color
  -> Word  -- ^ Key
  -> Zebra
  -> Word
findR d !x !w = go (Nil Black)
  where
    go !v t =
      case t of
        Bin p l r
          | w < p     -> go r l
          | otherwise -> go v r

        Bla k -> goTip Black k v
        Whi k -> goTip White k v

        Nil _ -> d

    goTip c k v =
      case w < k of
        True
          | c == x    -> k
          | otherwise -> w

        False
          | c == x    -> w
          | otherwise -> case v of
                           Nil _ -> d
                           _     -> let !(# kR #) = keyL v
                                    in kR




-- | \(\mathcal{O}(\min(n,W))\).
--   Set every key smaller than or equal to the given one to the given color.
fillL :: Word -> Color -> Zebra -> Zebra
fillL w x
  | w == maxBound = \_ -> Mono x
  | otherwise     = unsafeFillL (w + 1) x

-- | \(\mathcal{O}(\min(n,W))\).
--   Set every key smaller than the given one to the given color.
--
--   The given key __must not__ be @0@.
unsafeFillL :: Word -> Color -> Zebra -> Zebra
unsafeFillL w x = \t ->
  case fillL_ w x t of
    Nil _ -> Mono x
    t'    -> t'

fillL_ :: Word -> Color -> Zebra -> Zebra
fillL_ !w !x = go
  where
    go t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then rebinL p (go l) r
                   else let !(# cR #) = colorL l
                        in if cR == x
                             then let !(# cL #) = invert cR
                                  in join w (tip w cL) p t
                             else t

            else if w <= upper p
                   then go r
                   else let !(# cR #) = colorR r
                        in if cR == x
                             then Nil Black
                             else tip w cR

        Bla k -> goTip Black k t
        Whi k -> goTip White k t

        Nil _ -> t

    goTip c k t
      | w >= k    = if c == x
                      then Nil Black
                      else if w == k
                             then t
                             else tip w c

      | otherwise = if c == x
                      then let !(# cL #) = invert x
                           in join w (tip w cL) k t
                      else t



-- | \(\mathcal{O}(\min(n,W))\).
--   Set every key greater than or equal to the given one to the given color.
fillR :: Word -> Color -> Zebra -> Zebra
fillR w x = \t ->
  case fillR_ w x t of
    Nil _ -> Mono x
    t'    -> t'

fillR_ :: Word -> Color -> Zebra -> Zebra
fillR_ !w !x = go
  where
    go t =
      case t of
        Bin p l r ->
          if w < p
            then if w >= lower p
                   then go l
                   else let !(# cR #) = colorL l
                        in if cR == x
                             then tip w x
                             else Nil Black

            else if w <= upper p
                   then rebinR p l (go r)
                   else let !(# cR #) = colorR r
                        in if cR == x
                             then t
                             else join w (tip w x) p t

        Bla k -> goTip Black k t
        Whi k -> goTip White k t

        Nil _ -> t

    goTip c k t
      | w <= k    = if c == x
                      then if w == k
                             then t
                             else tip w c

                      else Nil Black

      | otherwise = if c == x
                      then t
                      else if k == 0
                             then tip w x
                             else join w (tip w x) k t



-- | \(\mathcal{O}(\min(n,W))\).
--   Set every key in the range to the given color.
fillRange :: Range -> Color -> Zebra -> Zebra
fillRange (UnsafeRange wL wR) x
  | wL == 0        = fillL wR x
  | wR == maxBound = fillR wL x
  | otherwise      = unsafeFillRange wL (wR + 1) x

-- | \(\mathcal{O}(\min(n,W) + n_L)\).
--   Set every key in the \([k_L, k_R)\) interval to the given color.
--
--   \(k_L\) __must not__ be @0@. \(k_R\) __must__ be greater than \(k_L\).
unsafeFillRange
  :: Word  -- ^ \(k_L\)
  -> Word  -- ^ \(k_R\)
  -> Color
  -> Zebra
  -> Zebra
unsafeFillRange wL wR x t =
  case fillRange_ x wL wR t of
    Nil _ -> Mono x
    t'    -> t'

fillRange_ :: Color -> Word -> Word -> Zebra -> Zebra
fillRange_ !x !wL !wR = go
  where
    !mM = branchingBit wL wR

    !pM = mask wL mM .|. mM

    binM = let !(# c #) = invert x
           in Bin pM (tip wL x) (tip wR c)

    go t =
      case t of
        Bin p l r ->
          case Prelude.compare p pM of
            EQ                 -> rebin p (fillR_ wL x l) (fillL_ wR x r)

            LT | pM <= upper p -> rebinR p l (go r)
               | p >= lower pM -> let l' = if wL < p
                                             then fillR_ wL x l
                                             else rebinR p l (fillR_ wL x r)

                                      !(# cR #) = colorR r

                                  in if cR == x
                                       then l'
                                       else join p l' pM (tip wR cR)

               | otherwise     ->
                   let !(# cR #) = colorR r
                   in if cR == x
                        then t
                        else join p t pM binM

            GT | p <= upper pM -> let r' = if wR >= p
                                             then fillL_ wR x r
                                             else rebinL p (fillL_ wR x l) r

                                      !(# cR #) = colorL l

                                  in if cR == x
                                       then join pM (tip wL x) p r'
                                       else r'

               | pM >= lower p -> rebinL p (go l) r
               | otherwise     ->
                   let !(# cR #) = colorL l
                   in if cR == x
                        then join p t pM binM
                        else t

        Bla k -> goTip k Black t
        Whi k -> goTip k White t

        Nil _ -> t

    goTip k c t
      | wR < k    = if c == x
                      then join k t pM binM
                      else t

      | k < wL    = if c == x
                      then t
                      else if k == 0
                             then binM
                             else join k t pM binM

      | c == x    = tip wL c
      | otherwise = tip wR c



colorL :: Zebra -> (# Color #)
colorL t =
  case t of
    Bin _ l _ -> colorL l
    Bla _     -> (# Black #)
    _         -> (# White #)

colorR :: Zebra -> (# Color #)
colorR t =
  case t of
    Bin _ _ r -> colorR r
    Bla _     -> (# Black #)
    _         -> (# White #)


keyL :: Zebra -> (# Word #)
keyL t =
  case t of
    Bin _ l _ -> keyL l
    Bla k     -> (# k #)
    Whi k     -> (# k #)
    Nil _     -> (# 0 #)

keyR :: Zebra -> (# Word #)
keyR t =
  case t of
    Bin _ _ r -> keyR r
    Bla k     -> (# k #)
    Whi k     -> (# k #)
    Nil _     -> (# 0 #)



-- | \(\mathcal{O}(n)\).
--   Invert the colors of all keys.
complement :: Zebra -> Zebra
complement t =
  case t of
    Bin p l r -> Bin p (Data.Zebra.Word.Internal.complement l)
                       (Data.Zebra.Word.Internal.complement r)
    Bla k     -> Whi k
    Whi k     -> Bla k
    Nil _     -> t



-- | \(\mathcal{O}(n_A + n_B)\).
--   Union of two trees over the given color.
union :: Color -> Zebra -> Zebra -> Zebra
union x l r =
  case l of
    Mono c | c == x    -> l
           | otherwise -> r

    _      ->
      case r of
        Mono c | c == x    -> r
               | otherwise -> l

        _      ->
          case anyAny l r of
            Nil _ -> Mono x
            t     -> t
  where
    anyAny tA tB =
      case tA of
        Bin pA lA rA -> binAny (# pA, lA, rA #) tA tB

        Bla kA -> tipAny (# kA, Black #) tA tB
        Whi kA -> tipAny (# kA, White #) tA tB

        Nil _ -> tA

    tipAny uA@(# kA, cA #) tA tB =
      case tB of
        Bin pB lB rB -> tipBin uA tA (# pB, lB, rB #) tB

        Bla kB -> goTip kB Black
        Whi kB -> goTip kB White

        Nil _ -> tB
      where
        goTip kB cB
          | cA == cB  = if (cA == x) == (kA < kB)
                          then tA
                          else tB

          | otherwise = if kA == kB || ((cA == x) == (kA < kB))
                          then Nil Black
                          else join kA tA kB tB

    binAny uA tA tB =
      case tB of
        Bin pB lB rB -> binBin uA tA (# pB, lB, rB #) tB

        Bla kB -> tipBin (# kB, Black #) tB uA tA
        Whi kB -> tipBin (# kB, White #) tB uA tA

        Nil _ -> tB

    tipBin uA@(# kA, cA #) tA (# pB, lB, rB #) tB =
      if kA < pB
        then if kA >= lower pB
               then if cA == x
                      then tipAny uA tA lB
                      else rebinL pB (tipAny uA tA lB) rB

               else let !(# cB #) = colorL lB
                    in if cA == cB
                         then if cA == x
                                then tA
                                else tB

                         else if cA == x
                                then Nil Black
                                else join kA tA pB tB

        else if kA <= upper pB
               then if cA == x
                      then rebinR pB lB (tipAny uA tA rB)
                      else tipAny uA tA rB

               else let !(# cB #) = colorR rB
                    in if cA == cB
                         then if cA == x
                                then tB
                                else tA

                         else if cA == x
                                then join kA tA pB tB
                                else Nil Black

    binBin uA@(# pA, lA, rA #) tA uB@(# pB, lB, rB #) tB =
      case Prelude.compare pA pB of
        EQ                  -> rebin pA (anyAny lA lB) (anyAny rA rB)

        LT | pB <= upper pA -> let !(# cR #) = colorL lB
                               in if cR == x
                                    then rebinR pA lA (binAny uB tB rA)
                                    else binAny uB tB rA

           | pA >= lower pB -> let !(# cL #) = colorR rA
                               in if cL == x
                                    then binAny uA tA lB
                                    else rebinL pB (binAny uA tA lB) rB

           | otherwise      ->
               let !(# cA #) = colorR rA
                   !(# cB #) = colorL lB

               in if cA == cB
                    then if cA == x
                           then tA
                           else tB

                    else if cA == x
                           then Nil Black
                           else join pA tA pB tB

        GT | pA <= upper pB -> let !(# cR #) = colorL lA
                               in if cR == x
                                    then rebinR pB lB (binAny uA tA rB)
                                    else binAny uA tA rB

           | pB >= lower pA -> let !(# cL #) = colorR rB
                               in if cL == x
                                    then binAny uB tB lA
                                    else rebinL pA (binAny uB tB lA) rA

           | otherwise      ->
               let !(# cB #) = colorR rB
                   !(# cA #) = colorL lA

               in if cA == cB
                    then if cA == x
                           then tB
                           else tA

                    else if cA == x
                           then join pA tA pB tB
                           else Nil Black



-- | \(\mathcal{O}(n_A + n_B)\).
--   Intersection of two trees over the given color.
intersection :: Color -> Zebra -> Zebra -> Zebra
intersection x =
  let !(# c #) = invert x
  in union c



-- | \(\mathcal{O}(n_A + n_B)\).
--   Determine whether two trees are disjoint over the given color.
disjoint :: Color -> Zebra -> Zebra -> Bool
disjoint x l r =
  case l of
    Mono c -> c /= x
    _      ->
      case r of
        Mono c -> c /= x
        _      -> anyAny l r
  where
    anyAny tA tB =
      case tA of
        Bin pA lA rA -> binAny (# pA, lA, rA #) tA tB

        Bla kA -> tipAny (# kA, Black #) tA tB
        Whi kA -> tipAny (# kA, White #) tA tB

        Nil _ -> False

    tipAny uA@(# kA, cA #) tA tB =
      case tB of
        Bin pB lB rB -> tipBin uA tA (# pB, lB, rB #)

        Bla kB -> goTip kB Black
        Whi kB -> goTip kB White

        Nil _ -> False
      where
        goTip kB cB
          | cA == cB  = False
          | otherwise = kA == kB || ((cA == x) == (kA < kB))

    binAny uA tA tB =
      case tB of
        Bin pB lB rB -> binBin uA tA (# pB, lB, rB #) tB

        Bla kB -> tipBin (# kB, Black #) tB uA
        Whi kB -> tipBin (# kB, White #) tB uA

        Nil _ -> False

    tipBin uA@(# kA, cA #) tA (# pB, lB, rB #) =
      if kA < pB
        then if kA >= lower pB
               then cA == x && tipAny uA tA lB

               else let !(# cB #) = colorL lB
                    in cA /= cB && cA == x

        else if kA <= upper pB
               then cA /= x && tipAny uA tA rB

               else let !(# cB #) = colorR rB
                    in cA /= cB && cB == x

    binBin uA@(# pA, lA, rA #) tA uB@(# pB, lB, rB #) tB =
      case Prelude.compare pA pB of
        EQ                  -> anyAny lA lB && anyAny rA rB

        LT | pB <= upper pA -> let !(# cR #) = colorL lB
                               in cR /= x && binAny uB tB rA

           | pA >= lower pB -> let !(# cL #) = colorR rA
                               in cL == x && binAny uA tA lB

           | otherwise      ->
               let !(# cA #) = colorR rA
                   !(# cB #) = colorL lB

               in cA /= cB && cA == x

        GT | pA <= upper pB -> let !(# cR #) = colorL lA
                               in cR /= x && binAny uA tA rB

           | pB >= lower pA -> let !(# cL #) = colorR rB
                               in cL == x && binAny uB tB lA

           | otherwise      ->
               let !(# cB #) = colorR rB
                   !(# cA #) = colorL lA

               in cA /= cB && cB == x



-- | \(\mathcal{O}(n_A + n_B)\).
--   Difference of two trees over the given color.
difference :: Color -> Zebra -> Zebra -> Zebra
difference x l r =
  case l of
    Mono c | c == x    -> Data.Zebra.Word.Internal.complement r
           | otherwise -> l

    _      ->
      case r of
        Mono c | c == x    -> let !(# x' #) = invert x
                              in Mono x'

               | otherwise -> l

        _      ->
          case anyAny L l r of
            Nil _ -> let !(# c #) = invert x
                     in Mono c

            t     -> t
  where
    anyAny s tA tB =
      case tA of
        Bin pA lA rA -> binAny s (# pA, lA, rA #) tA tB

        Bla kA -> tipAny s (# kA, Black #) tA tB
        Whi kA -> tipAny s (# kA, White #) tA tB

        Nil _ -> tA

    tipAny s uA@(# kA, cA #) tA tB =
      case tB of
        Bin pB lB rB -> tipBin s uA tA (# pB, lB, rB #) tB

        Bla kB -> goTip kB Black
        Whi kB -> goTip kB White

        Nil _ -> tB
      where
        goTip kB cB =
          case s of
            L -> goTipL kA cA tA kB cB
            R -> goTipL kB cB tB kA cA

        goTipL kL cL tL kR cR =
          case Prelude.compare kL kR of
            EQ -> if cL == cR
                    then Nil Black
                    else tL

            LT -> if cL == cR
                    then if cL == x
                           then let !(# c #) = invert x
                                in join kL tL kR (tip kR c)

                           else Nil Black

                    else if cL == x
                           then tip kR x
                           else tL

            GT -> if cL == cR
                    then if cL == x
                           then Nil Black
                           else join kL tL kR (tip kR x)

                    else if cL == x
                           then tL
                           else tip kR cL

    binAny s uA tA tB =
      case tB of
        Bin pB lB rB -> binBin s uA tA (# pB, lB, rB #) tB

        Bla kB -> goTip kB Black
        Whi kB -> goTip kB White

        Nil _ -> tB
      where
        goTip kB cB =
          let !(# s' #) = other s
          in tipBin s' (# kB, cB #) tB uA tA

    tipBin s uA@(# kA, cA #) tA (# pB, lB, rB #) tB =
      case s of
        L -> if kA < pB
               then if kA >= lower pB
                      then if cA == x
                             then rebinL pB (tipAny s uA tA lB)
                                            (Data.Zebra.Word.Internal.complement rB)

                             else tipAny s uA tA lB

                      else let !(# cR #) = colorL lB
                           in if cA == cR
                                then if cA == x
                                       then join kA tA
                                                 pB $ Bin pB (Data.Zebra.Word.Internal.complement lB)
                                                             (Data.Zebra.Word.Internal.complement rB)

                                       else Nil Black

                                else if cA == x
                                       then Bin pB (Data.Zebra.Word.Internal.complement lB)
                                                   (Data.Zebra.Word.Internal.complement rB)

                                       else tA

               else if kA <= upper pB
                      then if cA == x
                             then tipAny s uA tA rB
                             else rebinR pB (Data.Zebra.Word.Internal.complement lB)
                                            (tipAny s uA tA rB)

                      else let !(# cL #) = colorR rB
                           in if cA == cL
                                then if cA == x
                                       then Nil Black
                                       else join kA tA
                                                 pB $ Bin pB (Data.Zebra.Word.Internal.complement lB)
                                                             (Data.Zebra.Word.Internal.complement rB)

                                else if cA == x
                                       then tA
                                       else Bin pB (Data.Zebra.Word.Internal.complement lB)
                                                   (Data.Zebra.Word.Internal.complement rB)

        R -> if kA < pB
               then if kA >= lower pB
                      then if cA == x
                             then tipAny s uA tA lB
                             else rebinL pB (tipAny s uA tA lB) rB

                      else let !(# cR #) = colorL lB
                           in if cA == cR
                                then if cA == x
                                       then Nil Black
                                       else join kA (tip kA x) pB tB

                                else if cA == x
                                       then tip kA cR
                                       else tB

               else if kA <= upper pB
                      then if cA == x
                             then rebinR pB lB (tipAny s uA tA rB)
                             else tipAny s uA tA rB

                      else let !(# cL #) = colorR rB
                           in if cA == cL
                                then if cA == x
                                       then let !(# c #) = invert x
                                            in join kA (tip kA c) pB tB
                                       else Nil Black

                                else if cA == x
                                       then tB
                                       else tip kA cL

    binBin s uA@(# pA, lA, rA #) tA uB@(# pB, lB, rB #) tB =
      case Prelude.compare pA pB of
        EQ                  -> rebin pB (anyAny s lA lB) (anyAny s rA rB)

        LT | pB <= upper pA -> let !(# s' #) = other s

                                   !(# cR #) = colorL lB

                               in case s of
                                    L -> if cR == x
                                           then rebinR pA lA (binAny s' uB tB rA)
                                           else binAny s' uB tB rA

                                    R -> if cR == x
                                           then binAny s' uB tB rA
                                           else rebinR pA (Data.Zebra.Word.Internal.complement lA)
                                                          (binAny s' uB tB rA)

           | pA >= lower pB -> let !(# cL #) = colorR rA
                               in case s of
                                    L -> if cL == x
                                           then rebinL pB (binAny s uA tA lB)
                                                  (Data.Zebra.Word.Internal.complement rB)
                                           else binAny s uA tA lB

                                    R -> if cL == x
                                           then binAny s uA tA lB
                                           else rebinL pB (binAny s uA tA lB) rB

           | otherwise      ->
               let !(# cA #) = colorR rA
                   !(# cB #) = colorL lB

               in case s of
                    L -> if cA == cB
                           then if cA == x
                                  then join pA tA
                                            pB $ Bin pB (Data.Zebra.Word.Internal.complement lB)
                                                        (Data.Zebra.Word.Internal.complement rB)
                                  else Nil Black

                           else if cA == x
                                  then Bin pB (Data.Zebra.Word.Internal.complement lB)
                                              (Data.Zebra.Word.Internal.complement rB)
                                  else tA

                    R -> if cA == cB
                           then if cA == x
                                  then Nil Black
                                  else join pB tB
                                            pA $ Bin pA (Data.Zebra.Word.Internal.complement lA)
                                                        (Data.Zebra.Word.Internal.complement rA)

                           else if cA == x
                                  then Bin pA (Data.Zebra.Word.Internal.complement lA)
                                              (Data.Zebra.Word.Internal.complement rA)
                                  else tB

        GT | pA <= upper pB -> let !(# cR #) = colorL lA
                               in case s of
                                    L -> if cR == x
                                           then binAny s uA tA rB
                                           else rebinR pB
                                                  (Data.Zebra.Word.Internal.complement lB)
                                                  (binAny s uA tA rB)

                                    R -> if cR == x
                                           then rebinR pB lB (binAny s uA tA rB)
                                           else binAny s uA tA rB

           | pB >= lower pA -> let !(# s' #) = other s

                                   !(# cL #) = colorR rB

                               in case s of
                                    L -> if cL == x
                                           then binAny s' uB tB lA
                                           else rebinL pA (binAny s' uB tB lA) rA

                                    R -> if cL == x
                                           then rebinL pA (binAny s' uB tB lA)
                                                  (Data.Zebra.Word.Internal.complement rA)

                                           else binAny s' uB tB lA

           | otherwise      ->
               let !(# cB #) = colorR rB
                   !(# cA #) = colorL lA

               in case s of
                    L -> if cA == cB
                           then if cA == x
                                  then Nil Black
                                  else join pA tA
                                            pB $ Bin pB (Data.Zebra.Word.Internal.complement lB)
                                                        (Data.Zebra.Word.Internal.complement rB)

                           else if cA == x
                                  then tA
                                  else Bin pB (Data.Zebra.Word.Internal.complement lB)
                                              (Data.Zebra.Word.Internal.complement rB)

                    R -> if cA == cB
                           then if cA == x
                                  then join pB tB
                                            pA $ Bin pA (Data.Zebra.Word.Internal.complement lA)
                                                        (Data.Zebra.Word.Internal.complement rA)
                                  else Nil Black

                           else if cA == x
                                  then tB
                                  else Bin pA (Data.Zebra.Word.Internal.complement lA)
                                              (Data.Zebra.Word.Internal.complement rA)



-- | \(\mathcal{O}(n_A + n_B)\).
--   Symmetric difference of two trees over the given color.
symmetricDifference :: Color -> Zebra -> Zebra -> Zebra
symmetricDifference xFG l r =
  case l of
    Mono c | c == xFG  -> Data.Zebra.Word.Internal.complement r
           | otherwise -> r

    _      ->
      case r of
        Mono c | c == xFG  -> Data.Zebra.Word.Internal.complement l
               | otherwise -> l

        _      ->
          case anyAny l r of
            Nil c -> Mono c
            t     -> t
  where
    anyAny tA tB =
      case tA of
        Bin pA lA rA -> binAny (# pA, lA, rA #) tA tB

        Bla kA -> tipAny (# kA, Black #) tA tB
        Whi kA -> tipAny (# kA, White #) tA tB

        Nil _ -> tA

    tipAny uA@(# kA, cA #) tA tB =
      case tB of
        Bin pB lB rB -> tipBin uA tA (# pB, lB, rB #) tB

        Bla kB -> goTip kB Black
        Whi kB -> goTip kB White

        Nil _ -> tB
      where
        goTip kB cB
          | kA == kB  = Nil $ if cA == cB
                                then let !(# xBG #) = invert xFG
                                     in xBG
                                else xFG

          | otherwise = let nA | (cB == xFG) == (kA < kB) = tA
                               | otherwise                = let !(# c #) = invert cA
                                                            in tip kA c

                            nB | (cA == xFG) == (kA < kB) = let !(# c #) = invert cB
                                                            in tip kB c
                               | otherwise                = tB

                        in join kA nA kB nB

    binAny uA tA tB =
      case tB of
        Bin pB lB rB -> binBin uA tA (# pB, lB, rB #) tB

        Bla kB -> tipBin (# kB, Black #) tB uA tA
        Whi kB -> tipBin (# kB, White #) tB uA tA

        Nil _ -> tB

    tipBin uA@(# kA, cA #) tA (# pB, lB, rB #) tB =
      if kA < pB
        then if kA >= lower pB
               then let r' | cA == xFG = Data.Zebra.Word.Internal.complement rB
                           | otherwise = rB

                    in rebinL pB (tipAny uA tA lB) r'

               else let !(# cL #) = colorL lB

                        nA | cL == xFG = tA
                           | otherwise = let !(# c #) = invert cA
                                         in tip kA c

                        nB | cA == xFG = Bin pB (Data.Zebra.Word.Internal.complement lB)
                                                (Data.Zebra.Word.Internal.complement rB)
                           | otherwise = tB

                    in join kA nA pB nB

        else if kA <= upper pB
               then let l' | cA == xFG = lB
                           | otherwise = Data.Zebra.Word.Internal.complement lB

                    in rebinR pB l' (tipAny uA tA rB)

               else let !(# cR #) = colorR rB

                        nA | cR == xFG = let !(# c #) = invert cA
                                         in tip kA c
                           | otherwise = tA

                        nB | cA == xFG = tB
                           | otherwise = Bin pB (Data.Zebra.Word.Internal.complement lB)
                                                (Data.Zebra.Word.Internal.complement rB)

                    in join kA nA pB nB

    binBin uA@(# pA, lA, rA #) tA uB@(# pB, lB, rB #) tB =
      case Prelude.compare pA pB of
        EQ                  -> rebin pA (anyAny lA lB) (anyAny rA rB)

        LT | pB <= upper pA -> let !(# cR #) = colorL lB

                                   l' | cR == xFG = lA
                                      | otherwise = Data.Zebra.Word.Internal.complement lA

                               in rebinR pA l' (binAny uB tB rA)

           | pA >= lower pB -> let !(# cL #) = colorR rA

                                   r' | cL == xFG = Data.Zebra.Word.Internal.complement rB
                                      | otherwise = rB

                               in rebinL pB (binAny uA tA lB) r'

           | otherwise      ->
               let !(# cA #) = colorR rA
                   !(# cB #) = colorL lB

                   nA | cB == xFG = tA
                      | otherwise = Bin pA (Data.Zebra.Word.Internal.complement lA)
                                           (Data.Zebra.Word.Internal.complement rA)

                   nB | cA == xFG = Bin pB (Data.Zebra.Word.Internal.complement lB)
                                           (Data.Zebra.Word.Internal.complement rB)
                      | otherwise = tB

               in join pA nA pB nB

        GT | pA <= upper pB -> let !(# cR #) = colorL lA

                                   l' | cR == xFG = lB
                                      | otherwise = Data.Zebra.Word.Internal.complement lB

                               in rebinR pB l' (binAny uA tA rB)

           | pB >= lower pA -> let !(# cL #) = colorR rB

                                   r' | cL == xFG = Data.Zebra.Word.Internal.complement rA
                                      | otherwise = rA

                               in rebinL pA (binAny uB tB lA) r'

           | otherwise      ->
               let !(# cB #) = colorR rB
                   !(# cA #) = colorL lA

                   nA | cB == xFG = Bin pA (Data.Zebra.Word.Internal.complement lA)
                                           (Data.Zebra.Word.Internal.complement rA)
                      | otherwise = tA

                   nB | cA == xFG = tB
                      | otherwise = Bin pB (Data.Zebra.Word.Internal.complement lB)
                                           (Data.Zebra.Word.Internal.complement rB)
               in join pA nA pB nB



data S = L | R
         deriving Show

other :: S -> (# S #)
other L = (# R #)
other R = (# L #)

-- | \(\mathcal{O}(n_A + n_B)\).
--    Compare two trees with respect to set inclusion over the given color.
compare :: Color -> Zebra -> Zebra -> PartialOrdering
compare x l r =
  case l of
    Mono cA ->
      case r of
        Mono cB | cA == cB  -> Equal
                | cA == x   -> Superset
                | otherwise -> Subset

        _       | cA == x   -> Superset
                | otherwise -> Subset
    _      ->
      case r of
        Mono cB | cB == x   -> Subset
                | otherwise -> Superset

        _      -> anyAny L l r
  where
    anyAny s tA tB =
      case tA of
        Bin pA lA rA -> binAny s (# pA, lA, rA #) tA tB

        Bla kA -> tipAny s (# kA, Black #) tA tB
        Whi kA -> tipAny s (# kA, White #) tA tB

        Nil _ -> Incomparable

    tipAny s uA@(# kA, cA #) tA tB =
      case tB of
        Bin pB lB rB -> tipBin s uA tA (# pB, lB, rB #)

        Bla kB -> goTip kB Black
        Whi kB -> goTip kB White

        Nil _ -> Incomparable
      where
        goTip kB cB
          | cA == cB  = if kA == kB
                          then Equal
                          else if (cA == x) == (kA < kB)
                                 then case s of
                                        L -> Superset
                                        R -> Subset

                                 else case s of
                                        L -> Subset
                                        R -> Superset

          | otherwise = Incomparable

    binAny s uA tA tB =
      case tB of
        Bin pB lB rB -> binBin s uA tA (# pB, lB, rB #) tB

        Bla kB -> goTip kB Black
        Whi kB -> goTip kB White

        Nil _ -> Incomparable
      where
        goTip kB cB = let !(# s' #) = other s
                      in tipBin s' (# kB, cB #) tB uA

    tipBin s uA@(# kA, cA #) tA (# pB, lB, rB #) =
      if kA < pB
        then if kA >= lower pB
               then let !(# o #) = if cA == x
                                     then case s of
                                            L -> (# Superset #)
                                            R -> (# Subset #)

                                     else case s of
                                            L -> (# Subset #)
                                            R -> (# Superset #)

                    in order o (tipAny s uA tA lB)

               else let !(# cR #) = colorL lB
                    in if cA == cR
                         then if cA == x
                                then case s of
                                       L -> Superset
                                       R -> Subset

                                else case s of
                                       L -> Subset
                                       R -> Superset

                         else Incomparable

        else if kA <= upper pB
               then let !(# o #) = if cA == x
                                     then case s of
                                            L -> (# Subset #)
                                            R -> (# Superset #)

                                     else case s of
                                            L -> (# Superset #)
                                            R -> (# Subset #)

                    in order o (tipAny s uA tA rB)

               else let !(# cL #) = colorR rB
                    in if cA == cL
                         then if cA == x
                                then case s of
                                       L -> Subset
                                       R -> Superset

                                else case s of
                                       L -> Superset
                                       R -> Subset

                         else Incomparable

    binBin s uA@(# pA, lA, rA #) tA uB@(# pB, lB, rB #) tB =
      case Prelude.compare pA pB of
        EQ                  -> order (anyAny s lA lB) (anyAny s rA rB)

        LT | pB <= upper pA -> let !(# s' #) = other s

                                   !(# cR #) = colorL lB

                                   !(# o #) = if cR == x
                                                then case s of
                                                       L -> (# Superset #)
                                                       R -> (# Subset #)

                                                else case s of
                                                       L -> (# Subset #)
                                                       R -> (# Superset #)

                               in order o (binAny s' uB tB rA)

           | pA >= lower pB -> let !(# cL #) = colorR rA

                                   !(# o #) = if cL == x
                                                then case s of
                                                       L -> (# Superset #)
                                                       R -> (# Subset #)

                                                else case s of
                                                       L -> (# Subset #)
                                                       R -> (# Superset #)

                               in order o (binAny s uA tA lB)

           | otherwise      -> let !(# cL #) = colorR rA
                                   !(# cR #) = colorL lB

                               in if cL == cR
                                    then if cL == x
                                           then case s of
                                                  L -> Superset
                                                  R -> Subset

                                           else case s of
                                                  L -> Subset
                                                  R -> Superset

                                    else Incomparable

        GT | pA <= upper pB -> let !(# cR #) = colorL lA

                                   !(# o #) = if cR == x
                                                then case s of
                                                       L -> (# Subset #)
                                                       R -> (# Superset #)

                                                else case s of
                                                       L -> (# Superset #)
                                                       R -> (# Subset #)

                               in order o (binAny s uA tA rB)

           | pB >= lower pA -> let !(# s' #) = other s

                                   !(# cL #) = colorR rB

                                   !(# o #) = if cL == x
                                                then case s of
                                                       L -> (# Subset #)
                                                       R -> (# Superset #)

                                                else case s of
                                                       L -> (# Superset #)
                                                       R -> (# Subset #)

                               in order o (binAny s' uB tB lA)

           | otherwise      -> let !(# cL #) = colorR rB
                                   !(# cR #) = colorL lA

                               in if cL == cR
                                    then if cL == x
                                           then case s of
                                                  L -> Subset
                                                  R -> Superset

                                           else case s of
                                                  L -> Superset
                                                  R -> Subset

                                    else Incomparable
