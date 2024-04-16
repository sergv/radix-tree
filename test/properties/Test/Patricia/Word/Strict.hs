{-# LANGUAGE RankNTypes #-}

module Test.Patricia.Word.Strict
  ( test
  ) where

import           Data.Patricia.Word.Strict (Patricia)
import qualified Data.Patricia.Word.Strict as Pat
import           Data.Patricia.Word.Strict.Debug (validate, Validity (..))
import qualified Data.Patricia.Word.Strict.Unsafe as Pat
import           No.Tree (NoTree)
import qualified No.Tree as No
import           Test.Patricia.Word.Sample
import           Test.Kit

import           Data.Functor.Identity
import           Test.Hspec



patFromList :: [(Word, a)] -> Patricia a
patFromList = foldr (\(k, a) p -> Pat.insert k a p) Pat.empty

patToList :: Patricia a -> [(Word, a)]
patToList = Pat.foldrWithKey (\k a -> (:) (k, a)) []



patRange :: (Pat.Range -> a -> b) -> (Word, Word, a) -> b
patRange f (k1, k2, a) = f (Pat.Range k1 k2) a

patRange_ :: (Pat.Range -> b) -> (Word, Word) -> b
patRange_ f (k1, k2) = f (Pat.Range k1 k2)

noRange :: (No.Range Word -> a -> b) -> (Word, Word, a) -> b
noRange f (k1, k2, a) = f (No.WordRange No.Closed k1 No.Closed k2) a

noRange_ :: (No.Range Word -> b) -> (Word, Word) -> b
noRange_ f (k1, k2) = f (No.WordRange No.Closed k1 No.Closed k2)



unary0 :: [Case () (Patricia Int) (NoTree Word Int)]
unary0 = foldMap (mkUnary0 patFromList) [zero, one, tiny, small, medium] -- , large]

unary1 :: [Case (Word, Int) (Patricia Int) (NoTree Word Int)]
unary1 = foldMap (mkUnary1 patFromList) [zero, one, tiny, small, medium] -- , large]

unary1_ :: [Case Word (Patricia Int) (NoTree Word Int)]
unary1_ = augment fst unary1

unary2 :: [Case (Word, Word, Int) (Patricia Int) (NoTree Word Int)]
unary2 = foldMap (mkUnary2 patFromList) [zero, one, tiny, small, medium] -- , large]

unary2_ :: [Case (Word, Word) (Patricia Int) (NoTree Word Int)]
unary2_ = augment (\(k1, k2, _) -> (k1, k2)) unary2

binary
  , binaryL
  , subset
  , superset
  , equal
 :: [Case (Patricia Int, NoTree Word Int) (Patricia Int) (NoTree Word Int)]
binary   = foldMap (mkBinary   patFromList) [zero, one, tiny, small, medium] -- , large]
binaryL  = foldMap (mkBinaryL  patFromList) [zero, one, tiny, small, medium] -- , large]
subset   = foldMap (mkSubset   patFromList) [zero, one, tiny, small, medium] -- , large]
superset = foldMap (mkSuperset patFromList) [zero, one, tiny, small, medium] -- , large]
equal    = foldMap (mkEqual    patFromList) [zero, one, tiny, small, medium] -- , large]



type IdT s a b = Test s (Patricia a) (NoTree Word a) b b

type TreeT s a = Test s (Patricia a) (NoTree Word a) (Patricia a) (NoTree Word a)

treeEq :: Eq a => Patricia a -> NoTree Word a -> Bool
treeEq pat no =
  case validate pat of
    Valid -> patToList pat == No.toList no
    _     -> False

type SplitT s a =
       Test s (Patricia a) (NoTree Word a)
         (Pat.Split a a) (NoTree Word a, NoTree Word a)

splitEq
  :: Eq a => Pat.Split a a -> (NoTree Word a, NoTree Word a) -> Bool
splitEq (Pat.Split a b) (x, y) = treeEq a x && treeEq b y

type SplitLookupT s a =
       Test s (Patricia a) (NoTree Word a)
         (Pat.SplitLookup a a a) (NoTree Word a, Maybe a, NoTree Word a)

splitLookupEq
  :: Eq a
  => Pat.SplitLookup a a a -> (NoTree Word a, Maybe a, NoTree Word a) -> Bool
splitLookupEq (Pat.SplitLookup a b c) (x, y, z) = treeEq a x && b == y && treeEq c z

type LookupT s a =
       Test s (Patricia a) (NoTree Word a) (Maybe (Pat.Lookup a)) (Maybe (Word, a))

lookupEq :: Eq a => Maybe (Pat.Lookup a) -> Maybe (Word, a) -> Bool
lookupEq (Just (Pat.Lookup k a)) (Just (l, b)) = k == l && a == b
lookupEq Nothing                 Nothing       = True
lookupEq _                       _             = False

type MinViewT s a =
       Test s (Patricia a) (NoTree Word a)
         (Maybe (Pat.ViewL a)) (Maybe (Word, a, NoTree Word a))

minViewEq :: Eq a => Maybe (Pat.ViewL a) -> Maybe (Word, a, NoTree Word a) -> Bool
minViewEq (Just (Pat.ViewL (Pat.Lookup k a) pat)) (Just (l, b, no)) =
  k == l && a == b && treeEq pat no

minViewEq Nothing Nothing = True
minViewEq _       _       = False

type MaxViewT s a =
       Test s (Patricia a) (NoTree Word a)
         (Maybe (Pat.ViewR a)) (Maybe (NoTree Word a, Word, a))

maxViewEq :: Eq a => Maybe (Pat.ViewR a) -> Maybe (NoTree Word a, Word, a) -> Bool
maxViewEq (Just (Pat.ViewR pat (Pat.Lookup k a))) (Just (no, l, b)) =
  k == l && a == b && treeEq pat no

maxViewEq Nothing Nothing = True
maxViewEq _       _       = False



lookupT, dirtyLookupT :: Eq a => IdT Word a (Maybe a)
lookupT      = lookupT_ Pat.lookup
dirtyLookupT = lookupT_ Pat.dirtyLookup

lookupT_ :: Eq a => (forall x. Word -> Patricia x -> Maybe x) -> IdT Word a (Maybe a)
lookupT_ f = Test (==) f No.lookup

findT, dirtyFindT :: Eq a => IdT (Word, a) a a
findT      = findT_ Pat.find
dirtyFindT = findT_ Pat.dirtyFind

findT_ :: Eq a => (forall x. x -> Word -> Patricia x -> x) -> IdT (Word, a) a a
findT_ f = Test (==) (\(k, a) -> f a k) (\(k, a) -> No.find a k)

memberT, dirtyMemberT :: IdT Word a Bool
memberT      = memberT_ Pat.member
dirtyMemberT = memberT_ Pat.dirtyMember

memberT_ :: (forall x. Word -> Patricia x -> Bool) -> IdT Word a Bool
memberT_ f = Test (==) f No.member



insertT :: Eq a => TreeT (Word, a) a
insertT = Test treeEq (uncurry Pat.insert) (uncurry No.insert)

insertWithT, insertWithT' :: (Eq a, Integral a) => TreeT (Word, a) a
insertWithT  = insertWithT_ Pat.insertWith
insertWithT' = insertWithT_ Pat.insertWith'

insertWithT_
  :: (Eq a, Integral a)
  => (forall x. (x -> x) -> Word -> x -> Patricia x -> Patricia x) -> TreeT (Word, a) a
insertWithT_ g =
  let f x = (+ fromIntegral x)
  in Test treeEq (\(k, a) -> g (f a) k a) (\(k, a) -> No.insertWith (f a) k a)

adjustT, adjustT' :: (Eq a, Integral a) => TreeT (Word, a) a
adjustT  = adjustT_ Pat.adjust
adjustT' = adjustT_ Pat.adjust'

adjustT_
  :: (Eq a, Integral a)
  => (forall x. (x -> x) -> Word -> Patricia x -> Patricia x) -> TreeT (Word, a) a
adjustT_ g =
  let f a = (+ fromIntegral a)
  in Test treeEq (\(k, a) -> g (f a) k) (\(k, a) -> No.adjust (f a) k)

deleteT :: Eq a => TreeT Word a
deleteT = Test treeEq Pat.delete No.delete

updateAdjustT, updateDeleteT :: (Eq a, Integral a) => TreeT (Word, a) a
updateAdjustT = updateT_ (\a -> Just . (+ a))
updateDeleteT = updateT_ (\_ _ -> Nothing)

updateT_ :: Eq a => (a -> a -> Maybe a) -> TreeT (Word, a) a
updateT_ f = Test treeEq (\(k, a) -> Pat.update (f a) k) (\(k, a) -> No.update (f a) k)

alterInsertT
  , alterInsertWithT
  , alterAdjustT
  , alterDeleteT
 :: (Eq a, Integral a) => TreeT (Word, a) a
alterInsertT     = alterT_ (\a _ -> Just a)
alterInsertWithT = alterT_ (\a -> Just . maybe a (+ a))
alterAdjustT     = alterT_ (\a -> fmap (+ a))
alterDeleteT     = alterT_ (\_ _ -> Nothing)

alterT_ :: Eq a => (a -> Maybe a -> Maybe a) -> TreeT (Word, a) a
alterT_ f = Test treeEq (\(k, a) -> Pat.alter (f a) k) (\(k, a) -> No.alter (f a) k)



splitLT :: Eq a => SplitT Word a
splitLT = Test splitEq Pat.splitL (No.splitL No.Closed)

splitRT :: Eq a => SplitT Word a
splitRT = Test splitEq Pat.splitR No.splitR

splitLookupT :: Eq a => SplitLookupT Word a
splitLookupT = Test splitLookupEq Pat.splitLookup No.splitLookup



lookupLT :: Eq a => LookupT Word a
lookupLT = Test lookupEq Pat.lookupL (No.lookupL No.Closed)

adjustLT, adjustLT' :: (Eq a, Integral a) => TreeT (Word, a) a
adjustLT  = adjustLT_ Pat.adjustL
adjustLT' = adjustLT_ Pat.adjustL'

adjustLT_
  :: (Eq a, Integral a)
  => (forall x. (x -> x) -> Word -> Patricia x -> Patricia x)
  -> TreeT (Word, a) a
adjustLT_ g =
  let f a = (+ a)
  in Test treeEq (\(k, a) -> g (f a) k) (\(k, a) -> No.adjustL (f a) No.Closed k)

adjustLWithKeyT
  , adjustLWithKeyT'
 :: (Eq a, Integral a) => TreeT (Word, a) a
adjustLWithKeyT  = adjustLWithKeyT_ Pat.adjustLWithKey
adjustLWithKeyT' = adjustLWithKeyT_ Pat.adjustLWithKey'

adjustLWithKeyT_
  :: (Eq a, Integral a)
  => (forall x. (Word -> x -> x) -> Word -> Patricia x -> Patricia x)
  -> TreeT (Word, a) a
adjustLWithKeyT_ g =
  let f a k = (+ fromIntegral k) . (+ a)
  in Test treeEq (\(k, a) -> g (f a) k) (\(k, a) -> No.adjustLWithKey (f a) No.Closed k)

deleteLT :: Eq a => TreeT (Word) a
deleteLT = Test treeEq Pat.deleteL (No.deleteL No.Closed)

updateLAdjustT
  , updateLDeleteT
 :: (Eq a, Integral a) => TreeT (Word, a) a
updateLAdjustT = updateLT_ (\a -> Just . (+ a))
updateLDeleteT = updateLT_ (\_ _ -> Nothing)

updateLT_
  :: (Eq a, Integral a)
  => (a -> a -> Maybe a) -> TreeT (Word, a) a
updateLT_ f =
  Test treeEq (\(k, a) -> Pat.updateL (f a) k) (\(k, a) -> No.updateL (f a) No.Closed k)

updateLWithKeyAdjustT
  , updateLWithKeyDeleteT
 :: (Eq a, Integral a) => TreeT (Word, a) a
updateLWithKeyAdjustT = updateLWithKeyT_ (\a k -> Just . (+ fromIntegral k) . (+ a))
updateLWithKeyDeleteT = updateLWithKeyT_ (\_ _ _ -> Nothing)

updateLWithKeyT_
  :: (Eq a, Integral a)
  => (a -> Word -> a -> Maybe a) -> TreeT (Word, a) a
updateLWithKeyT_ f =
  Test treeEq (\(k, a) -> Pat.updateLWithKey (f a) k)
              (\(k, a) -> No.updateLWithKey (f a) No.Closed k)

takeLT :: Eq a => TreeT (Word) a
takeLT = Test treeEq Pat.takeL (No.takeL No.Closed)



lookupRT :: Eq a => LookupT (Word) a
lookupRT = Test lookupEq Pat.lookupR (No.lookupR No.Closed)

adjustRT, adjustRT' :: (Eq a, Integral a) => TreeT (Word, a) a
adjustRT  = adjustRT_ Pat.adjustR
adjustRT' = adjustRT_ Pat.adjustR'

adjustRT_
  :: (Eq a, Integral a)
  => (forall x. (x -> x) -> Word -> Patricia x -> Patricia x)
  -> TreeT (Word, a) a
adjustRT_ g =
  let f a = (+ a)
  in Test treeEq (\(k, a) -> g (f a) k) (\(k, a) -> No.adjustR (f a) No.Closed k)

adjustRWithKeyT
  , adjustRWithKeyT'
 :: (Eq a, Integral a) => TreeT (Word, a) a
adjustRWithKeyT  = adjustRWithKeyT_ Pat.adjustRWithKey
adjustRWithKeyT' = adjustRWithKeyT_ Pat.adjustRWithKey'

adjustRWithKeyT_
  :: (Eq a, Integral a)
  => (forall x. (Word -> x -> x) -> Word -> Patricia x -> Patricia x)
  -> TreeT (Word, a) a
adjustRWithKeyT_ g =
  let f a k = (+ fromIntegral k) . (+ a)
  in Test treeEq (\(k, a) -> g (f a) k) (\(k, a) -> No.adjustRWithKey (f a) No.Closed k)

deleteRT :: Eq a => TreeT (Word) a
deleteRT = Test treeEq Pat.deleteR (No.deleteR No.Closed)

updateRAdjustT
  , updateRDeleteT
 :: (Eq a, Integral a) => TreeT (Word, a) a
updateRAdjustT = updateRT_ (\a -> Just . (+ a))
updateRDeleteT = updateRT_ (\_ _ -> Nothing)

updateRT_
  :: (Eq a, Integral a)
  => (a -> a -> Maybe a) -> TreeT (Word, a) a
updateRT_ f =
  Test treeEq (\(k, a) -> Pat.updateR (f a) k) (\(k, a) -> No.updateR (f a) No.Closed k)

updateRWithKeyAdjustT
  , updateRWithKeyDeleteT
 :: (Eq a, Integral a) => TreeT (Word, a) a
updateRWithKeyAdjustT = updateRWithKeyT_ (\a k -> Just . (+ fromIntegral k) . (+ a))
updateRWithKeyDeleteT = updateRWithKeyT_ (\_ _ _ -> Nothing)

updateRWithKeyT_
  :: (Eq a, Integral a)
  => (a -> Word -> a -> Maybe a) -> TreeT (Word, a) a
updateRWithKeyT_ f =
  Test treeEq (\(k, a) -> Pat.updateRWithKey (f a) k)
              (\(k, a) -> No.updateRWithKey (f a) No.Closed k)

takeRT :: Eq a => TreeT (Word) a
takeRT = Test treeEq Pat.takeR (No.takeR No.Closed)



adjustRangeT
  , adjustRangeT'
 :: (Eq a, Integral a) => TreeT (Word, Word, a) a
adjustRangeT  = adjustRangeT_ Pat.adjustRange
adjustRangeT' = adjustRangeT_ Pat.adjustRange'

adjustRangeT_
  :: (Eq a, Integral a)
  => (forall x. (x -> x) -> Pat.Range -> Patricia x -> Patricia x)
  -> TreeT (Word, Word, a) a
adjustRangeT_ g =
  let f a = (+ a)
  in Test treeEq (patRange $ \r a -> g (f a) r) (noRange $ \r a -> No.adjustRange (f a) r)

adjustRangeWithKeyT
  , adjustRangeWithKeyT'
 :: (Eq a, Integral a) => TreeT (Word, Word, a) a
adjustRangeWithKeyT  = adjustRangeWithKeyT_ Pat.adjustRangeWithKey
adjustRangeWithKeyT' = adjustRangeWithKeyT_ Pat.adjustRangeWithKey'

adjustRangeWithKeyT_
  :: (Eq a, Integral a)
  => (forall x. (Word -> x -> x) -> Pat.Range -> Patricia x -> Patricia x)
  -> TreeT (Word, Word, a) a
adjustRangeWithKeyT_ g =
  let f a k = (+ fromIntegral k) . (+ a)
  in Test treeEq
       (patRange $ \r a -> g (f a) r) (noRange $ \r a -> No.adjustRangeWithKey (f a) r)

deleteRangeT :: Eq a => TreeT (Word, Word) a
deleteRangeT = Test treeEq (patRange_ Pat.deleteRange) (noRange_ No.deleteRange)

updateRangeAdjustT
  , updateRangeDeleteT
 :: (Eq a, Integral a) => TreeT (Word, Word, a) a
updateRangeAdjustT = updateRangeT_ (\a -> Just . (+ a))
updateRangeDeleteT = updateRangeT_ (\_ _ -> Nothing)

updateRangeT_
  :: (Eq a, Integral a)
  => (a -> a -> Maybe a) -> TreeT (Word, Word, a) a
updateRangeT_ f =
  Test treeEq
    (patRange $ \r a -> Pat.updateRange (f a) r) (noRange $ \r a -> No.updateRange (f a) r)

updateRangeWithKeyAdjustT
  , updateRangeWithKeyDeleteT
 :: (Eq a, Integral a) => TreeT (Word, Word, a) a
updateRangeWithKeyAdjustT = updateRangeWithKeyT_ (\a k -> Just . (+ fromIntegral k) . (+ a))
updateRangeWithKeyDeleteT = updateRangeWithKeyT_ (\_ _ _ -> Nothing)

updateRangeWithKeyT_
  :: (Eq a, Integral a)
  => (a -> Word -> a -> Maybe a) -> TreeT (Word, Word, a) a
updateRangeWithKeyT_ f =
  Test treeEq (patRange $ \r a -> Pat.updateRangeWithKey (f a) r)
              (noRange $ \r a -> No.updateRangeWithKey (f a) r)



takeRangeT :: Eq a => TreeT (Word, Word) a
takeRangeT = Test treeEq (patRange_ Pat.takeRange) (noRange_ No.takeRange)



lookupMinT :: Eq a => IdT () a (Maybe a)
lookupMinT = Test (==) (\_ -> Pat.lookupMin) (\_ -> No.lookupMin)

lookupMinWithKeyT :: Eq a => LookupT () a
lookupMinWithKeyT = Test lookupEq (\_ -> Pat.lookupMinWithKey) (\_ -> No.lookupMinWithKey)

adjustMinT, adjustMinT' :: (Eq a, Integral a) => TreeT () a
adjustMinT  = adjustMinT_ Pat.adjustMin
adjustMinT' = adjustMinT_ Pat.adjustMin'

adjustMinT_
  :: (Eq a, Integral a)
  => (forall x. (x -> x) -> Patricia x -> Patricia x) -> TreeT () a
adjustMinT_ f = Test treeEq (\_ -> f (+ 10000)) (\_ -> No.adjustMin (+ 10000))

adjustMinWithKeyT, adjustMinWithKeyT' :: (Eq a, Integral a) => TreeT () a
adjustMinWithKeyT  = adjustMinWithKeyT_ Pat.adjustMinWithKey
adjustMinWithKeyT' = adjustMinWithKeyT_ Pat.adjustMinWithKey'

adjustMinWithKeyT_
  :: (Eq a, Integral a)
  => (forall x. (Word -> x -> x) -> Patricia x -> Patricia x) -> TreeT () a
adjustMinWithKeyT_ g =
  let f k = (+ fromIntegral k)
  in Test treeEq (\_ -> g f) (\_ -> No.adjustMinWithKey f)

deleteMinT :: (Eq a, Integral a) => TreeT () a
deleteMinT = Test treeEq (\_ -> Pat.deleteMin) (\_ -> No.deleteMin)

updateMinAdjustT, updateMinDeleteT :: (Eq a, Integral a) => TreeT () a
updateMinAdjustT = updateMinT_ (Just . (+ 10000))
updateMinDeleteT = updateMinT_ (\_ -> Nothing)

updateMinT_ :: (Eq a, Integral a) => (a -> Maybe a) -> TreeT () a
updateMinT_ f = Test treeEq (\_ -> Pat.updateMin f) (\_ -> No.updateMin f)

updateMinWithKeyAdjustT, updateMinWithKeyDeleteT :: (Eq a, Integral a) => TreeT () a
updateMinWithKeyAdjustT = updateMinWithKeyT_ (\k -> Just . (+ fromIntegral k))
updateMinWithKeyDeleteT = updateMinWithKeyT_ (\_ _ -> Nothing)

updateMinWithKeyT_ :: (Eq a, Integral a) => (Word -> a -> Maybe a) -> TreeT () a
updateMinWithKeyT_ f =
  Test treeEq (\_ -> Pat.updateMinWithKey f) (\_ -> No.updateMinWithKey f)

minViewT :: Eq a => MinViewT () a
minViewT = Test minViewEq (\_ -> Pat.minView) (\_ -> No.minView)



lookupMaxT :: Eq a => IdT () a (Maybe a)
lookupMaxT = Test (==) (\_ -> Pat.lookupMax) (\_ -> No.lookupMax)

lookupMaxWithKeyT :: Eq a => LookupT () a
lookupMaxWithKeyT = Test lookupEq (\_ -> Pat.lookupMaxWithKey) (\_ -> No.lookupMaxWithKey)

adjustMaxT, adjustMaxT' :: (Eq a, Integral a) => TreeT () a
adjustMaxT  = adjustMaxT_ Pat.adjustMax
adjustMaxT' = adjustMaxT_ Pat.adjustMax'

adjustMaxT_
  :: (Eq a, Integral a)
  => (forall x. (x -> x) -> Patricia x -> Patricia x) -> TreeT () a
adjustMaxT_ f = Test treeEq (\_ -> f (+ 10000)) (\_ -> No.adjustMax (+ 10000))

adjustMaxWithKeyT, adjustMaxWithKeyT' :: (Eq a, Integral a) => TreeT () a
adjustMaxWithKeyT  = adjustMaxWithKeyT_ Pat.adjustMaxWithKey
adjustMaxWithKeyT' = adjustMaxWithKeyT_ Pat.adjustMaxWithKey'

adjustMaxWithKeyT_
  :: (Eq a, Integral a)
  => (forall x. (Word -> x -> x) -> Patricia x -> Patricia x) -> TreeT () a
adjustMaxWithKeyT_ g =
  let f k = (+ fromIntegral k)
  in Test treeEq (\_ -> g f) (\_ -> No.adjustMaxWithKey f)

deleteMaxT :: (Eq a, Integral a) => TreeT () a
deleteMaxT = Test treeEq (\_ -> Pat.deleteMax) (\_ -> No.deleteMax)

updateMaxAdjustT, updateMaxDeleteT :: (Eq a, Integral a) => TreeT () a
updateMaxAdjustT = updateMaxT_ (Just . (+ 10000))
updateMaxDeleteT = updateMaxT_ (\_ -> Nothing)

updateMaxT_ :: (Eq a, Integral a) => (a -> Maybe a) -> TreeT () a
updateMaxT_ f = Test treeEq (\_ -> Pat.updateMax f) (\_ -> No.updateMax f)

updateMaxWithKeyAdjustT, updateMaxWithKeyDeleteT :: (Eq a, Integral a) => TreeT () a
updateMaxWithKeyAdjustT = updateMaxWithKeyT_ (\k -> Just . (+ fromIntegral k))
updateMaxWithKeyDeleteT = updateMaxWithKeyT_ (\_ _ -> Nothing)

updateMaxWithKeyT_ :: (Eq a, Integral a) => (Word -> a -> Maybe a) -> TreeT () a
updateMaxWithKeyT_ f =
  Test treeEq (\_ -> Pat.updateMaxWithKey f) (\_ -> No.updateMaxWithKey f)

maxViewT :: Eq a => MaxViewT () a
maxViewT = Test maxViewEq (\_ -> Pat.maxView) (\_ -> No.maxView)



eqT :: (Eq a, Integral a) => IdT (Pat.Patricia a, No.NoTree Word a) a Bool
eqT = Test (==) (\(a, _) b -> a == b) (\(_, a) b -> a == b)

filterT :: (Eq a, Integral a) => TreeT () a
filterT = Test treeEq (\_ -> Pat.filter odd) (\_ -> No.filter odd)

filterWithKeyT :: (Eq a, Integral a) => TreeT () a
filterWithKeyT =
  let f k a = odd $ fromIntegral k + a
  in Test treeEq (\_ -> Pat.filterWithKey f) (\_ -> No.filterWithKey f)

mapMaybeT :: (Eq a, Integral a) => TreeT () a
mapMaybeT =
  let f a | odd a     = Nothing
          | otherwise = Just a

  in Test treeEq (\_ -> Pat.mapMaybe f) (\_ -> No.mapMaybe f)

mapMaybeWithKeyT :: (Eq a, Integral a) => TreeT () a
mapMaybeWithKeyT =
  let f k a | odd (fromIntegral k + a) = Nothing
            | otherwise                = Just a

  in Test treeEq (\_ -> Pat.mapMaybeWithKey f) (\_ -> No.mapMaybeWithKey f)

partitionT :: (Eq a, Integral a) => SplitT () a
partitionT = Test splitEq (\_ -> Pat.partition odd) (\_ -> No.partition odd)

partitionWithKeyT :: (Eq a, Integral a) => SplitT () a
partitionWithKeyT =
  let f k a = odd $ fromIntegral k + a
  in Test splitEq (\_ -> Pat.partitionWithKey f) (\_ -> No.partitionWithKey f)

mapEitherT :: (Eq a, Integral a) => SplitT () a
mapEitherT =
  let f a | odd a     = Left a
          | otherwise = Right a

  in Test splitEq (\_ -> Pat.mapEither f) (\_ -> No.mapEither f)

mapEitherWithKeyT :: (Eq a, Integral a) => SplitT () a
mapEitherWithKeyT =
  let f k a | odd (fromIntegral k + a) = Left a
            | otherwise                = Right a

  in Test splitEq (\_ -> Pat.mapEitherWithKey f) (\_ -> No.mapEitherWithKey f)



mapT, mapT' :: (Eq a, Num a) => TreeT () a
mapT  = mapT_ Pat.map
mapT' = mapT_ Pat.map'

mapT_ :: (Eq a, Num a) => (forall x. (x -> x) -> Patricia x -> Patricia x) -> TreeT () a
mapT_ g =
  let f = (+ 10000)
  in Test treeEq (\_ -> g f) (\_ -> No.map f)

mapWithKeyT, mapWithKeyT' :: (Eq a, Num a) => TreeT () a
mapWithKeyT  = mapWithKeyT_ Pat.mapWithKey
mapWithKeyT' = mapWithKeyT_ Pat.mapWithKey'

mapWithKeyT_
  :: (Eq a, Num a)
  => (forall x. (Word -> x -> x) -> Patricia x -> Patricia x) -> TreeT () a
mapWithKeyT_ g =
  let f k = (+ fromIntegral k) . (+ 10000)
  in Test treeEq (\_ -> g f) (\_ -> No.mapWithKey f)



sizeT :: IdT () a Int
sizeT = Test (==) (\_ -> Pat.size) (\_ -> No.size)

foldlT, foldlT' :: Eq a => IdT () a [a]
foldlT  = foldlT_ Pat.foldl
foldlT' = foldlT_ Pat.foldl'

foldlT_ :: Eq a => (forall x. (x -> a -> x) -> x -> Patricia a -> x) -> IdT () a [a]
foldlT_ g = Test (==) (\_ -> g (flip (:)) []) (\_ -> No.foldl (flip (:)) [])

foldlWithKeyT, foldlWithKeyT' :: Eq a => IdT () a [(Word, a)]
foldlWithKeyT  = foldlWithKeyT_ Pat.foldlWithKey
foldlWithKeyT' = foldlWithKeyT_ Pat.foldlWithKey'

foldlWithKeyT_
  :: Eq a
  => (forall x. (x -> Word -> a -> x) -> x -> Patricia a -> x) -> IdT () a [(Word, a)]
foldlWithKeyT_ g =
  let f z k a = (k, a) : z
  in Test (==) (\_ -> g f []) (\_ -> No.foldlWithKey f [])



foldrT, foldrT' :: Eq a => IdT () a [a]
foldrT  = foldrT_ Pat.foldr
foldrT' = foldrT_ Pat.foldr'

foldrT_
  :: Eq a => (forall x. (a -> x -> x) -> x -> Patricia a -> x) -> IdT () a [a]
foldrT_ g = Test (==) (\_ -> g (:) []) (\_ -> No.foldr (:) [])

foldrWithKeyT, foldrWithKeyT' :: Eq a => IdT () a [(Word, a)]
foldrWithKeyT  = foldrWithKeyT_ Pat.foldrWithKey
foldrWithKeyT' = foldrWithKeyT_ Pat.foldrWithKey'

foldrWithKeyT_
  :: Eq a
  => (forall x. (Word -> a -> x -> x) -> x -> Patricia a -> x) -> IdT () a [(Word, a)]
foldrWithKeyT_ g =
  let f k a = (:) (k, a)
  in Test (==) (\_ -> g f []) (\_ -> No.foldrWithKey f [])



foldMapT :: Eq a => IdT () a [a]
foldMapT = Test (==) (\_ -> Pat.foldMap pure) (\_ -> No.foldMap pure)

foldMapWithKeyT :: Eq a => IdT () a [(Word, a)]
foldMapWithKeyT =
  let f k a = [(k, a)]
  in Test (==) (\_ -> Pat.foldMapWithKey f) (\_ -> No.foldMapWithKey f)



idTreeEq :: Eq a => Identity (Patricia a) -> Identity (NoTree Word a) -> Bool
idTreeEq (Identity a) (Identity b) = treeEq a b

traverseT
  :: (Eq a, Num a)
  => Test s (Patricia a) (NoTree Word a) (Identity (Patricia a)) (Identity (NoTree Word a))
traverseT =
  let f = Identity . (+ 10000)
  in Test idTreeEq (\_ -> Pat.traverse f) (\_ -> No.traverse f)

traverseWithKeyT
  :: (Eq a, Num a)
  => Test s (Patricia a) (NoTree Word a) (Identity (Patricia a)) (Identity (NoTree Word a))
traverseWithKeyT =
  let f k a = Identity $ fromIntegral k + 10000 + a
  in Test idTreeEq (\_ -> Pat.traverseWithKey f) (\_ -> No.traverseWithKey f)



unionT :: Eq a => TreeT (Patricia a, NoTree Word a) a
unionT = Test treeEq (\(a, _) b -> Pat.union a b) (\(_, a) b -> No.unionL a b)

unionLT :: Eq a => TreeT (Patricia a, NoTree Word a) a
unionLT = Test treeEq (\(a, _) b -> Pat.unionL a b) (\(_, a) b -> No.unionL a b)

unionWithT' :: (Eq a, Num a) => TreeT (Patricia a, NoTree Word a) a
unionWithT' = Test treeEq (\(a, _) b -> Pat.unionWith' (+) a b)
                          (\(_, a) b -> No.unionWithKey (\_ -> (+)) a b)

unionWithKeyT'
  , mergeUnionT
 :: (Eq a, Num a) => TreeT (Patricia a, NoTree Word a) a
unionWithKeyT' = unionWithKeyT_ Pat.unionWithKey'
mergeUnionT    =
  unionWithKeyT_ $ \f ->
    Pat.merge
      (\k a b -> Pat.Tip k $ f k a b)
      Pat.Tip Pat.Bin Pat.Tip Pat.Bin

unionWithKeyT_
  :: (Eq a, Num a)
  => (forall x. (Word -> x -> x -> x) -> Patricia x -> Patricia x -> Patricia x)
  -> TreeT (Patricia a, NoTree Word a) a
unionWithKeyT_ g =
  let f k a b = fromIntegral k + a + b
  in Test treeEq (\(a, _) b -> g f a b)
                 (\(_, a) b -> No.unionWithKey f a b)



differenceT :: Eq a => TreeT (Patricia a, NoTree Word a) a
differenceT = Test treeEq (\(a, _) b -> Pat.difference a b)
                          (\(_, a) b -> No.difference a b)

differenceWithT :: (Eq a, Integral a) => TreeT (Patricia a, NoTree Word a) a
differenceWithT =
  let f a b = let c = a + b
              in if odd c
                   then Nothing
                   else Just c

  in Test treeEq (\(a, _) b -> Pat.differenceWith f a b)
                 (\(_, a) b -> No.differenceWithKey (\_ -> f) a b)

differenceWithKeyT
  , mergeDifferenceT
 :: (Eq a, Integral a) => TreeT (Patricia a, NoTree Word a) a
differenceWithKeyT = differenceWithKeyT_ Pat.differenceWithKey
mergeDifferenceT    =
  differenceWithKeyT_ $ \f ->
    Pat.merge
      (\k a b -> case f k a b of
                   Just c  -> Pat.Tip k c
                   Nothing -> Pat.Nil
      )
      Pat.Tip Pat.Bin
      (\_ _ -> Pat.Nil) (\_ _ _ -> Pat.Nil)

differenceWithKeyT_
  :: (Eq a, Integral a)
  => (forall x y. (Word -> x -> y -> Maybe x) -> Patricia x -> Patricia y -> Patricia x)
  -> TreeT (Patricia a, NoTree Word a) a
differenceWithKeyT_ g =
  let f k a b = let c = fromIntegral k + a + b
                in if odd c
                     then Nothing
                     else Just c

  in Test treeEq (\(a, _) b -> g f a b)
                 (\(_, a) b -> No.differenceWithKey f a b)



disjointT :: IdT (Patricia a, NoTree Word a) a Bool
disjointT = Test (==) (\(a, _) b -> Pat.disjoint a b)
                      (\(_, a) b -> No.null $ No.intersectionL a b)

intersectionT :: Eq a => TreeT (Patricia a, NoTree Word a) a
intersectionT = Test treeEq (\(a, _) b -> Pat.intersection a b)
                             (\(_, a) b -> No.intersectionL a b)

intersectionLT :: Eq a => TreeT (Patricia a, NoTree Word a) a
intersectionLT = Test treeEq (\(a, _) b -> Pat.intersectionL a b)
                             (\(_, a) b -> No.intersectionL a b)

intersectionWithT' :: (Eq a, Num a) => TreeT (Patricia a, NoTree Word a) a
intersectionWithT' = Test treeEq (\(a, _) b -> Pat.intersectionWith' (+) a b)
                                 (\(_, a) b -> No.intersectionWithKey (\_ -> (+)) a b)

intersectionWithKeyT'
  , mergeIntersectionT
 :: (Eq a, Num a) => TreeT (Patricia a, NoTree Word a) a
intersectionWithKeyT' = intersectionWithKeyT_ Pat.intersectionWithKey'
mergeIntersectionT    =
  intersectionWithKeyT_ $ \f ->
    Pat.merge
      (\k a b -> Pat.Tip k $ f k a b)
      (\_ _ -> Pat.Nil) (\_ _ _ -> Pat.Nil)
      (\_ _ -> Pat.Nil) (\_ _ _ -> Pat.Nil)

intersectionWithKeyT_
  :: (Eq a, Num a)
  => (forall x y z. (Word -> x -> y -> z) -> Patricia x -> Patricia y -> Patricia z)
  -> TreeT (Patricia a, NoTree Word a) a
intersectionWithKeyT_ g =
  let f k a b = fromIntegral k + a + b
  in Test treeEq (\(a, _) b -> g f a b)
                 (\(_, a) b -> No.intersectionWithKey f a b)



compareT :: Eq a => IdT (Patricia a, NoTree Word a) a Pat.PartialOrdering
compareT = Test (==) (\(a, _) b -> Pat.compare (==) a b)
                     (\(_, a) b -> No.compare a b)



test :: Spec
test = do
  describe "Single-key" $ do
    it "lookup"           $ run unary1_ lookupT
    it "dirtyLookup"      $ run unary1_ dirtyLookupT
    it "find"             $ run unary1  findT
    it "dirtyFind"        $ run unary1  dirtyFindT
    it "member"           $ run unary1_ memberT
    it "dirtyMember"      $ run unary1_ dirtyMemberT
    it "insert"           $ run unary1  insertT
    it "insertWith"       $ run unary1  insertWithT
    it "insertWith'"      $ run unary1  insertWithT'
    it "adjust"           $ run unary1  adjustT
    it "adjust'"          $ run unary1  adjustT'
    it "delete"           $ run unary1_ deleteT
    it "update/adjust"    $ run unary1  updateAdjustT
    it "update/delete"    $ run unary1  updateDeleteT
    it "alter/insert"     $ run unary1  alterInsertT
    it "alter/insertWith" $ run unary1  alterInsertWithT
    it "alter/adjust"     $ run unary1  alterAdjustT
    it "alter/delete"     $ run unary1  alterDeleteT

  describe "Split" $ do
    it "splitL"           $ run unary1_ splitLT
    it "splitR"           $ run unary1_ splitRT
    it "splitLookup"      $ run unary1_ splitLookupT

  describe "Left" $ do
    it "lookupL"               $ run unary1_ lookupLT
    it "adjustL"               $ run unary1  adjustLT
    it "adjustL'"              $ run unary1  adjustLT'
    it "adjustLWithKey"        $ run unary1  adjustLWithKeyT
    it "adjustLWithKey'"       $ run unary1  adjustLWithKeyT'
    it "deleteL"               $ run unary1_ deleteLT
    it "updateL/adjust"        $ run unary1  updateLAdjustT
    it "updateL/delete"        $ run unary1  updateLDeleteT
    it "updateLWithKey/adjust" $ run unary1  updateLWithKeyAdjustT
    it "updateLWithKey/delete" $ run unary1  updateLWithKeyDeleteT
    it "takeL"                 $ run unary1_ takeLT

  describe "Right" $ do
    it "lookupR"               $ run unary1_ lookupRT
    it "adjustR"               $ run unary1  adjustRT
    it "adjustR'"              $ run unary1  adjustRT'
    it "adjustRWithKey"        $ run unary1  adjustRWithKeyT
    it "adjustRWithKey'"       $ run unary1  adjustRWithKeyT'
    it "deleteR"               $ run unary1_ deleteRT
    it "updateR/adjust"        $ run unary1  updateRAdjustT
    it "updateR/delete"        $ run unary1  updateRDeleteT
    it "updateRWithKey/adjust" $ run unary1  updateRWithKeyAdjustT
    it "updateRWithKey/delete" $ run unary1  updateRWithKeyDeleteT
    it "takeR"                 $ run unary1_ takeRT

  describe "Range" $ do
    it "adjustRange"               $ run unary2  adjustRangeT
    it "adjustRange'"              $ run unary2  adjustRangeT'
    it "adjustRangeWithKey"        $ run unary2  adjustRangeWithKeyT
    it "adjustRangeWithKey'"       $ run unary2  adjustRangeWithKeyT'
    it "deleteRange"               $ run unary2_ deleteRangeT
    it "updateRange/adjust"        $ run unary2  updateRangeAdjustT
    it "updateRange/delete"        $ run unary2  updateRangeDeleteT
    it "updateRangeWithKey/adjust" $ run unary2  updateRangeWithKeyAdjustT
    it "updateRangeWithKey/delete" $ run unary2  updateRangeWithKeyDeleteT
    it "takeRange"                 $ run unary2_ takeRangeT

  describe "Min" $ do
    it "lookupMin"               $ run unary0 lookupMinT
    it "lookupMinWithKey"        $ run unary0 lookupMinWithKeyT
    it "adjustMin"               $ run unary0 adjustMinT
    it "adjustMinWithKey"        $ run unary0 adjustMinWithKeyT
    it "adjustMin'"              $ run unary0 adjustMinT'
    it "adjustMinWithKey'"       $ run unary0 adjustMinWithKeyT'
    it "deleteMin"               $ run unary0 deleteMinT
    it "updateMin/adjust"        $ run unary0 updateMinAdjustT
    it "updateMin/delete"        $ run unary0 updateMinDeleteT
    it "updateMinWithKey/adjust" $ run unary0 updateMinWithKeyAdjustT
    it "updateMinWithKey/delete" $ run unary0 updateMinWithKeyDeleteT
    it "minView"                 $ run unary0 minViewT

  describe "Max" $ do
    it "lookupMax"               $ run unary0 lookupMaxT
    it "lookupMaxWithKey"        $ run unary0 lookupMaxWithKeyT
    it "adjustMax"               $ run unary0 adjustMaxT
    it "adjustMaxWithKey"        $ run unary0 adjustMaxWithKeyT
    it "adjustMax'"              $ run unary0 adjustMaxT'
    it "adjustMaxWithKey'"       $ run unary0 adjustMaxWithKeyT'
    it "deleteMax"               $ run unary0 deleteMaxT
    it "updateMax/adjust"        $ run unary0 updateMaxAdjustT
    it "updateMax/delete"        $ run unary0 updateMaxDeleteT
    it "updateMaxWithKey/adjust" $ run unary0 updateMaxWithKeyAdjustT
    it "updateMaxWithKey/delete" $ run unary0 updateMaxWithKeyDeleteT
    it "maxView"                 $ run unary0 maxViewT

  describe "Partition" $ do
    it "filter"           $ run unary0 filterT
    it "filterWithKey"    $ run unary0 filterWithKeyT
    it "mapMaybe"         $ run unary0 mapMaybeT
    it "mapMaybeWithKey"  $ run unary0 mapMaybeWithKeyT
    it "partition"        $ run unary0 partitionT
    it "partitionWithKey" $ run unary0 partitionWithKeyT
    it "mapEither"        $ run unary0 mapEitherT
    it "mapEitherWithKey" $ run unary0 mapEitherWithKeyT

  describe "Full-tree" $ do
    it "(==)"            $ run (equal <> binaryL) eqT
    it "map"             $ run unary0 mapT
    it "map'"            $ run unary0 mapT'
    it "mapWithKey"      $ run unary0 mapWithKeyT
    it "mapWithKey'"     $ run unary0 mapWithKeyT'
    it "size"            $ run unary0 sizeT
    it "foldl"           $ run unary0 foldlT
    it "foldl'"          $ run unary0 foldlT'
    it "foldlWithKey"    $ run unary0 foldlWithKeyT
    it "foldlWithKey'"   $ run unary0 foldlWithKeyT'
    it "foldr"           $ run unary0 foldrT
    it "foldr'"          $ run unary0 foldrT'
    it "foldrWithKey"    $ run unary0 foldrWithKeyT
    it "foldrWithKey'"   $ run unary0 foldrWithKeyT'
    it "foldMap"         $ run unary0 foldMapT
    it "foldMapWithKey"  $ run unary0 foldMapWithKeyT
    it "traverse"        $ run unary0 traverseT
    it "traverseWithKey" $ run unary0 traverseWithKeyT

  describe "Merge" $ do
    it "union"                $ run binary  unionT
    it "unionL"               $ run binaryL unionLT
    it "unionWith'"           $ run binaryL unionWithT'
    it "unionWithKey'"        $ run binaryL unionWithKeyT'
    it "difference"           $ run binaryL differenceT
    it "differenceWith"       $ run binaryL differenceWithT
    it "differenceWithKey"    $ run binaryL differenceWithKeyT
    it "disjoint/yes"         $ run binary  disjointT
    it "disjoint/no"          $ run binaryL disjointT
    it "intersection"         $ run binary  intersectionT
    it "intersectionL"        $ run binaryL intersectionLT
    it "intersectionWith'"    $ run binaryL intersectionWithT'
    it "intersectionWithKey'" $ run binaryL intersectionWithKeyT'
    it "compare/subset"       $ run subset   compareT
    it "compare/superset"     $ run superset compareT
    it "compare/equal"        $ run equal    compareT
    it "compare/incomparable" $ run binary   compareT
    it "merge/union"          $ run binaryL mergeUnionT
    it "merge/difference"     $ run binaryL mergeDifferenceT
    it "merge/intersection"   $ run binaryL mergeIntersectionT
