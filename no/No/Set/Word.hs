{-# LANGUAGE DerivingStrategies
           , GeneralizedNewtypeDeriving
           , PatternSynonyms
           , ViewPatterns #-}

module No.Set.Word
  ( Color (..)
  , other

  , NoSet (Mono, ..)

  , No.Set.Word.lookup
  , lookupL
  , findL
  , lookupR
  , findR

  , Range (..)
  , monoL
  , monoR
  , monoRange

  , size
  , sizeL
  , sizeR
  , sizeRange

  , fillL
  , fillR
  , fillRange

  , No.Set.Word.foldl
  , No.Set.Word.foldl'
  , No.Set.Word.foldr
  , No.Set.Word.foldr'

  , foldlL
  , foldlL'
  , foldrL
  , foldrL'

  , foldlR
  , foldlR'
  , foldrR
  , foldrR'

  , foldlRange
  , foldlRange'
  , foldrRange
  , foldrRange'

  , complement

  , union
  , disjoint
  , intersection

  , difference
  , symmetricDifference

  , PartialOrdering (..)
  , No.Set.Word.compare
  ) where


import           Data.Zebra.Word (Color (..), PartialOrdering (..))
import           Data.Zebra.Word.Unsafe (Range (..))

import           Data.Foldable
import           Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import           Numeric.Natural



other :: Color -> Color
other Black = White
other White = Black



newtype NoSet = NoSet { getNoSet :: Seq (Color, Word, Word) }
                deriving newtype Eq

instance Show NoSet where
  showsPrec _ (NoSet xs) =
    showList . fmap (\(_, kL, kR) -> (kL, kR))
             . filter (\(c, _, _) -> c == White)
             $ toList xs

pattern Mono :: Color -> NoSet
pattern Mono c <- (monotone -> Just c)
  where
    Mono c = NoSet $ Seq.singleton (c, 0, maxBound)

monotone :: NoSet -> Maybe Color
monotone (NoSet (Seq.Empty Seq.:|> (c, _, _))) = Just c
monotone _                                     = Nothing



lookup :: Word -> NoSet -> Color
lookup w no =
  let ~(NoSet l, _) = unsafeSplitL w no
  in case l of
       _ :|> (c, _, _) -> c
       Empty           ->
         error $ "No.Set.Word.lookup: out of bounds (" <> shows w ")"



lookupL :: Color -> Word -> NoSet -> Maybe Word
lookupL x w no =
  let ~(NoSet l, _) = unsafeSplitL w no
  in case l of
       _ :|> _ -> go l
       Empty   ->
         error $ "No.Set.Word.lookupL: out of bounds (" <> shows w ")"

  where
    go (l :|> (c, _, b)) | c == x    = Just b
                         | otherwise = go l
    go Empty = Nothing

findL :: Word -> Color -> Word -> NoSet -> Word
findL d x w no =
  case lookupL x w no of
    Just a  -> a
    Nothing -> d



lookupR :: Color -> Word -> NoSet -> Maybe Word
lookupR x w no =
  let ~(_, NoSet r) = unsafeSplitR w no
  in case r of
       _ :<| _ -> go r
       Empty   ->
         error $ "No.Set.Word.lookupR: out of bounds (" <> shows w ")"

  where
    go ((c, a, _) :<| r) | c == x    = Just a
                         | otherwise = go r
    go Empty = Nothing

findR :: Word -> Color -> Word -> NoSet -> Word
findR d x w no =
  case lookupR x w no of
    Just a  -> a
    Nothing -> d



monoL :: Word -> NoSet -> Maybe Color
monoL k = monotone . fst . unsafeSplitL k

monoR :: Word -> NoSet -> Maybe Color
monoR k = monotone . snd . unsafeSplitR k

monoRange :: Range -> NoSet -> Maybe Color
monoRange r = monotone . (\(_, m, _) -> m) . unsafeSplitRange r



size :: Color -> NoSet -> Natural
size x =
  let f (c, a, b) z
        | c == x    = fromIntegral (b - a) + 1 + z
        | otherwise = z

  in Data.Foldable.foldr f 0 . getNoSet

sizeL :: Color -> Word -> NoSet -> Natural
sizeL x k = size x . fst . unsafeSplitL k

sizeR :: Color -> Word -> NoSet -> Natural
sizeR x k = size x . snd . unsafeSplitR k

sizeRange :: Color -> Range -> NoSet -> Natural
sizeRange x r = size x . (\(_, m, _) -> m) . unsafeSplitRange r



fillL :: Word -> Color -> NoSet -> NoSet
fillL w x no =
  let ~(_, NoSet _r) = unsafeSplitL w no
  in case _r of
       (c, _, b) :<| r | c == x    -> NoSet $ (c, 0, b) :<| r
                       | otherwise -> NoSet $ (x, 0, w) :<| _r

       _                           -> Mono x

fillR :: Word -> Color -> NoSet -> NoSet
fillR w x no =
  let ~(NoSet _l, _) = unsafeSplitR w no
  in case _l of
       l :|> (c, a, _) | c == x    -> NoSet $ l :|> (c, a, maxBound)
                       | otherwise -> NoSet $ _l :|> (x, w, maxBound)

       _                           -> Mono x

fillRange :: Range -> Color -> NoSet -> NoSet
fillRange rng@(Range kL kR) x no =
  let ~(NoSet _l, _, NoSet _r) = unsafeSplitRange rng no
  in case (_l, _r) of
       (l :|> (cL, a, _), (cR, _, b) :<| r) ->
         case (cL == cR, cL == x) of
           (True , True ) -> NoSet $ l <> ((x, a, b) :<| r)
           (True , False) -> NoSet $ _l <> ((x, kL, kR) :<| _r)
           (False, True ) -> NoSet $ (l :|> (x, a, kR)) <> _r
           (False, False) -> NoSet $ _l <> ((x, a, b) :<| r)

       (l :|> (cL, a, _), Empty)
         | cL == x   -> NoSet $ l :|> (cL, a, maxBound)
         | otherwise -> NoSet $ _l :|> (x, kL, maxBound)

       (Empty, (cR, _, b) :<| r)
         | cR == x   -> NoSet $ (cR, 0, b) :<| r
         | otherwise -> NoSet $ (x, 0, kR) :<| _r

       (Empty, Empty) -> Mono x



unsafeSplitL :: Word -> NoSet -> (NoSet, NoSet)
unsafeSplitL k (NoSet xs) =
  let ~(_l, r) = Seq.spanl (\(_, a, _) -> a <= k) xs
  in case _l of
       l :|> (c, a, b) | b > k -> (NoSet $ l :|> (c, a, k), NoSet $ (c, k + 1, b) :<| r)
       _                       -> (NoSet _l, NoSet r)

unsafeSplitR :: Word -> NoSet -> (NoSet, NoSet)
unsafeSplitR k (NoSet xs) =
  let ~(_r, l) = Seq.spanr (\(_, _, b) -> b >= k) xs
  in case _r of
      (c, a, b) :<| r | a < k -> (NoSet $ l :|> (c, a, k - 1), NoSet $ (c, k, b) :<| r)
      _                       -> (NoSet l, NoSet _r)

unsafeSplitRange :: Range -> NoSet -> (NoSet, NoSet, NoSet)
unsafeSplitRange (Range kL kR) no =
  let ~(l, no') = unsafeSplitR kL no
      ~(m, r)   = unsafeSplitL kR no'
  in (l, m, r)



foldl, foldl' :: (a -> Range -> Color -> a) -> a -> NoSet -> a
foldl  f z0 = Data.Foldable.foldl  (\z (c, a, b) -> f z (UnsafeRange a b) c) z0 . getNoSet
foldl' f z0 = Data.Foldable.foldl' (\z (c, a, b) -> f z (UnsafeRange a b) c) z0 . getNoSet

foldr, foldr' :: (Range -> Color -> a -> a) -> a -> NoSet -> a
foldr  f z0 = Data.Foldable.foldr  (\(c, a, b) -> f (UnsafeRange a b) c) z0 . getNoSet
foldr' f z0 = Data.Foldable.foldr' (\(c, a, b) -> f (UnsafeRange a b) c) z0 . getNoSet



foldlL, foldlL' :: Word -> (a -> Range -> Color -> a) -> a -> NoSet -> a
foldlL  w f z = No.Set.Word.foldl  f z . fst . unsafeSplitL w
foldlL' w f z = No.Set.Word.foldl' f z . fst . unsafeSplitL w

foldrL, foldrL' :: Word -> (Range -> Color -> a -> a) -> a -> NoSet -> a
foldrL  w f z = No.Set.Word.foldr  f z . fst . unsafeSplitL w
foldrL' w f z = No.Set.Word.foldr' f z . fst . unsafeSplitL w



foldlR, foldlR' :: Word -> (a -> Range -> Color -> a) -> a -> NoSet -> a
foldlR  w f z = No.Set.Word.foldl  f z . snd . unsafeSplitR w
foldlR' w f z = No.Set.Word.foldl' f z . snd . unsafeSplitR w

foldrR, foldrR' :: Word -> (Range -> Color -> a -> a) -> a -> NoSet -> a
foldrR  w f z = No.Set.Word.foldr  f z . snd . unsafeSplitR w
foldrR' w f z = No.Set.Word.foldr' f z . snd . unsafeSplitR w



foldlRange, foldlRange' :: Range -> (a -> Range -> Color -> a) -> a -> NoSet -> a
foldlRange  r f z = No.Set.Word.foldl  f z . (\(_, m, _) -> m) . unsafeSplitRange r
foldlRange' r f z = No.Set.Word.foldl' f z . (\(_, m, _) -> m) . unsafeSplitRange r

foldrRange, foldrRange' :: Range -> (Range -> Color -> a -> a) -> a -> NoSet -> a
foldrRange  r f z = No.Set.Word.foldr  f z . (\(_, m, _) -> m) . unsafeSplitRange r
foldrRange' r f z = No.Set.Word.foldr' f z . (\(_, m, _) -> m) . unsafeSplitRange r



-- | Combines two sets into an ascending non-overlapping list of
--   consecutive double-colored ranges.
--
--   Both sets must be defined over the same ranges for this function to make sense.
crush :: NoSet -> NoSet -> [(Color, Range, Color)]
crush (NoSet xs) (NoSet ys) = go xs ys
  where
    go Empty                Empty                = []
    go ((cL, aL, bL) :<| l) ((cR, aR, bR) :<| r) =
      case bL `Prelude.compare` bR of
        LT -> (cL, UnsafeRange aL bL, cR) : go l ((cR, bL + 1, bR) :<| r)
        GT -> (cL, UnsafeRange aR bR, cR) : go ((cL, bR + 1, bL) :<| l) r
        EQ -> (cL, UnsafeRange aL bL, cR) : go l r

    go _ _ =
      error "No.Set.Word.crush: unequally sized sets"



complement :: NoSet -> NoSet
complement = NoSet . fmap (\(c, a, b) -> (other c, a, b)) . getNoSet



union :: Color -> NoSet -> NoSet -> NoSet
union x =
  merge $ \cL cR ->
    if cL == cR && cL /= x
      then cL
      else x

disjoint :: Color -> NoSet -> NoSet -> Bool
disjoint x a b =
  case intersection x a b of
    Mono y -> x /= y
    _      -> False

intersection :: Color -> NoSet -> NoSet -> NoSet
intersection x =
  merge $ \cL cR ->
    if cL == cR && cL == x
      then x
      else other x

difference :: Color -> NoSet -> NoSet -> NoSet
difference x =
  merge $ \cL cR ->
    if cL /= cR && cL == x
      then x
      else other x

symmetricDifference :: Color -> NoSet -> NoSet -> NoSet
symmetricDifference x =
  merge $ \cL cR ->
    if cL == cR
      then other x
      else x




data Carry = Carry Color Word
           | NoCarry

merge :: (Color -> Color -> Color) -> NoSet -> NoSet -> NoSet
merge f as bs = NoSet . Seq.fromList . unify NoCarry $ crush as bs
  where
    unify carry [] =
      case carry of
        NoCarry   -> []
        Carry c k -> (c, k, maxBound) : []

    unify carry ((cL, Range a _, cR) : rest) =
      let cM = f cL cR
      in case carry of
           NoCarry   -> unify (Carry cM a) rest
           Carry c k
             | c == cM   -> unify carry rest
             | otherwise -> (c, k, a - 1) : unify (Carry cM a) rest



compare :: Color -> NoSet -> NoSet -> PartialOrdering
compare x as bs = Data.Foldable.foldr go Equal $ crush as bs
  where
    go (cL, _, cR) p =
      case p of
        Subset
          | cL == cR || cR == x -> Subset
          | otherwise           -> Incomparable

        Superset
          | cL == cR || cL == x -> Superset
          | otherwise           -> Incomparable

        Equal
          | cL == cR  -> Equal
          | cR == x   -> Subset
          | otherwise -> Superset

        Incomparable -> Incomparable
