{-# LANGUAGE RankNTypes #-}

module Test.RadixTree.Word8.Lazy
  ( test
  ) where

import qualified Data.Radix1Tree.Word8.Lazy as Radix1
import           Data.RadixTree.Word8.Lazy (RadixTree)
import qualified Data.RadixTree.Word8.Lazy as Radix
import           Data.RadixTree.Word8.Lazy.Debug
import qualified Data.RadixTree.Word8.Lazy.Unsafe as Radix
import           No.Tree (NoTree)
import qualified No.Tree as No
import           Test.Kit
import           Test.RadixNTree.Word8.Sample

import           Data.Functor.Identity
import qualified Data.List as List
import           Data.Word
import           Test.Hspec



radixFromList :: [([Word8], a)] -> RadixTree a
radixFromList = foldr (\(k, a) p -> Radix.insert (Radix.feedBytes k) a p) Radix.empty

radixToList :: RadixTree a -> [([Word8], a)]
radixToList = Radix.foldrWithKey (\k a -> (:) (Radix.buildBytes k, a)) []



unary0 :: [Case () (RadixTree Int) (NoTree [Word8] Int)]
unary0 = foldMap (mkUnary0 radixFromList) [zero, one, tip, bin, tiny, small, medium]

unary1F :: [Case (No.Openness, [Word8], Int) (RadixTree Int) (NoTree [Word8] Int)]
unary1F = foldMap (mkUnary1 radixFromList) [zero, one, tip, bin, tiny, small, medium]

unary1R :: [Case (No.Openness, [Word8]) (RadixTree Int) (NoTree [Word8] Int)]
unary1R = augment (\(o, k, _) -> (o, k)) unary1F

unary1 :: [Case ([Word8], Int) (RadixTree Int) (NoTree [Word8] Int)]
unary1 = augment (\(_, k, i) -> (k, i)) unary1F

unary1_ :: [Case [Word8] (RadixTree Int) (NoTree [Word8] Int)]
unary1_ = augment (\(_, k, _) -> k) unary1F



binary :: [Case (RadixTree Int, NoTree [Word8] Int) (RadixTree Int) (NoTree [Word8] Int)]
binary = foldMap (mkBinary radixFromList) [zero, one, tip, bin, tiny, small, medium]

binaryL :: [Case (RadixTree Int, NoTree [Word8] Int) (RadixTree Int) (NoTree [Word8] Int)]
binaryL = foldMap (mkBinaryL radixFromList) [zero, one, tip, bin, tiny, small, medium]

equal :: [Case (RadixTree Int, NoTree [Word8] Int) (RadixTree Int) (NoTree [Word8] Int)]
equal = foldMap (mkEqual radixFromList) [zero, one, tip, bin, tiny, small, medium]

subset :: [Case (RadixTree Int, NoTree [Word8] Int) (RadixTree Int) (NoTree [Word8] Int)]
subset = foldMap (mkSubset radixFromList) [zero, one, tip, bin, tiny, small, medium]

superset :: [Case (RadixTree Int, NoTree [Word8] Int) (RadixTree Int) (NoTree [Word8] Int)]
superset = foldMap (mkSuperset radixFromList) [zero, one, tip, bin, tiny, small, medium]



type IdT s a b = Test s (RadixTree a) (NoTree [Word8] a) b b

type TreeT s a = Test s (RadixTree a) (NoTree [Word8] a) (RadixTree a) (NoTree [Word8] a)

treeEq :: Eq a => RadixTree a -> NoTree [Word8] a -> Bool
treeEq pat no =
  case validate pat of
    Valid -> radixToList pat == No.toList no
    _     -> False

type SplitT s a =
       Test s (RadixTree a) (NoTree [Word8] a)
         (RadixTree a, RadixTree a) (NoTree [Word8] a, NoTree [Word8] a)

splitEq
  :: Eq a => (RadixTree a, RadixTree a) -> (NoTree [Word8] a, NoTree [Word8] a) -> Bool
splitEq (a, b) (x, y) = treeEq a x && treeEq b y

type SplitLookupT s a =
       Test s (RadixTree a) (NoTree [Word8] a)
         (RadixTree a, Maybe a, RadixTree a) (NoTree [Word8] a, Maybe a, NoTree [Word8] a)

splitLookupEq
  :: Eq a
  => (RadixTree a, Maybe a, RadixTree a)
  -> (NoTree [Word8] a, Maybe a, NoTree [Word8] a) -> Bool
splitLookupEq (a, b, c) (x, y, z) = treeEq a x && b == y && treeEq c z

type LookupT s a =
       Test s (RadixTree a) (NoTree [Word8] a)
       (Maybe (Radix.Lookup a)) (Maybe ([Word8], a))

lookupEq :: Eq a => Maybe (Radix.Lookup a) -> Maybe ([Word8], a) -> Bool
lookupEq (Just (Radix.Lookup k a)) (Just (l, b)) = Radix.buildBytes k == l && a == b
lookupEq Nothing                   Nothing       = True
lookupEq _                         _             = False

type MinViewT s a =
       Test s (RadixTree a) (NoTree [Word8] a)
         (Maybe (Radix.ViewL a)) (Maybe ([Word8], a, NoTree [Word8] a))

minViewEq :: Eq a => Maybe (Radix.ViewL a) -> Maybe ([Word8], a, NoTree [Word8] a) -> Bool
minViewEq (Just (Radix.ViewL k a t)) (Just (l, b, no)) =
  Radix.buildBytes k == l && a == b && treeEq t no

minViewEq Nothing                    Nothing           = True
minViewEq _                          _                 = False

type MaxViewT s a =
       Test s (RadixTree a) (NoTree [Word8] a)
         (Maybe (Radix.ViewR a)) (Maybe (NoTree [Word8] a, [Word8], a))

maxViewEq :: Eq a => Maybe (Radix.ViewR a) -> Maybe (NoTree [Word8] a, [Word8], a) -> Bool
maxViewEq (Just (Radix.ViewR t k a)) (Just (no, l, b)) =
  Radix.buildBytes k == l && a == b && treeEq t no

maxViewEq Nothing                    Nothing           = True
maxViewEq _                          _                 = False



lookupT :: Eq a => IdT [Word8] a (Maybe a)
lookupT = Test (==) (Radix.lookup . Radix.feedBytes) No.lookup

findT :: Eq a => IdT ([Word8], a) a a
findT = Test (==) (\(k, i) -> Radix.find i $ Radix.feedBytes k) (\(k, i) -> No.find i k)

memberT :: Eq a => IdT [Word8] a Bool
memberT = Test (==) (Radix.member . Radix.feedBytes) No.member

subtreeT :: Eq a => TreeT [Word8] a
subtreeT = Test treeEq (Radix.subtree . Radix.feedBytes) No.subtree

moveSingleT :: Eq a => IdT [Word8] a (Maybe a)
moveSingleT =
  Test (==) (\k -> Radix.stop . Radix.move (Radix.feedBytes k) . Radix.cursor)
            No.lookup

moveThirdsT :: Eq a => IdT [Word8] a (Maybe a)
moveThirdsT =
  let thirds xs = let len = length xs
                      ~(as, ys) = List.splitAt (len `quot` 3) xs
                      ~(bs, cs) = List.splitAt (len `quot` 3) ys

                  in Radix.move (Radix.feedBytes cs)
                   . Radix.move (Radix.feedBytes bs)
                   . Radix.move (Radix.feedBytes as)

  in Test (==) (\k -> Radix.stop . thirds k . Radix.cursor) No.lookup



prefixT :: Eq a => TreeT [Word8] a
prefixT = Test treeEq (Radix.prefix . Radix.feedBytes) No.prefix

insertT :: Eq a => TreeT ([Word8], a) a
insertT = Test treeEq (\(k, i) -> Radix.insert (Radix.feedBytes k) i) (uncurry No.insert)

insertWithT :: (Eq a, Integral a) => TreeT ([Word8], a) a
insertWithT  = insertWithT_ Radix.insertWith

insertWithT_
  :: (Eq a, Integral a)
  => (forall x. (x -> x) -> Radix.Feed -> x -> RadixTree x -> RadixTree x) -> TreeT ([Word8], a) a
insertWithT_ g =
  let f x = (+ fromIntegral x)
  in Test treeEq (\(k, a) -> g (f a) (Radix.feedBytes k) a)
                 (\(k, a) -> No.insertWith (f a) k a)

adjustT :: (Eq a, Integral a) => TreeT ([Word8], a) a
adjustT  = adjustT_ Radix.adjust

adjustT_
  :: (Eq a, Integral a)
  => (forall x. (x -> x) -> Radix.Feed -> RadixTree x -> RadixTree x) -> TreeT ([Word8], a) a
adjustT_ g =
  let f a = (+ fromIntegral a)
  in Test treeEq (\(k, a) -> g (f a) (Radix.feedBytes k))
                 (\(k, a) -> No.adjust (f a) k)

deleteT :: Eq a => TreeT [Word8] a
deleteT = Test treeEq (Radix.delete . Radix.feedBytes) No.delete

pruneT :: Eq a => TreeT (No.Openness, [Word8]) a
pruneT = Test treeEq (\(o, k) -> Radix.prune o $ Radix.feedBytes k) (uncurry No.prune)

updateAdjustT, updateDeleteT :: (Eq a, Integral a) => TreeT ([Word8], a) a
updateAdjustT = updateT_ (\a -> Just . (+ a))
updateDeleteT = updateT_ (\_ _ -> Nothing)

updateT_ :: Eq a => (a -> a -> Maybe a) -> TreeT ([Word8], a) a
updateT_ f = Test treeEq (\(k, a) -> Radix.update (f a) (Radix.feedBytes k))
                         (\(k, a) -> No.update (f a) k)

alterInsertT
  , alterInsertWithT
  , alterAdjustT
  , alterDeleteT
 :: (Eq a, Integral a) => TreeT ([Word8], a) a
alterInsertT     = alterT_ (\a _ -> Just a)
alterInsertWithT = alterT_ (\a -> Just . maybe a (+ a))
alterAdjustT     = alterT_ (\a -> fmap (+ a))
alterDeleteT     = alterT_ (\_ _ -> Nothing)

alterT_ :: Eq a => (a -> Maybe a -> Maybe a) -> TreeT ([Word8], a) a
alterT_ f = Test treeEq (\(k, a) -> Radix.alter (f a) (Radix.feedBytes k))
                        (\(k, a) -> No.alter (f a) k)

shapeInsertT :: (Eq a, Integral a) => TreeT [Word8] a
shapeInsertT =
  Test treeEq
    (Radix.shape (Radix.insert (Radix.feedBytes [1, 2, 3]) 10000) . Radix.feedBytes)
    (No.shape (No.insert [1, 2, 3] 10000))

shapeAdjustT :: (Eq a, Integral a) => TreeT [Word8] a
shapeAdjustT = Test treeEq (Radix.shape (Radix.map negate) . Radix.feedBytes)
                           (No.shape (No.map negate))

shapeFilterT :: (Eq a, Integral a) => TreeT [Word8] a
shapeFilterT = Test treeEq (Radix.shape (Radix.filter odd) . Radix.feedBytes)
                           (No.shape (No.filter odd))

shapeDeleteT :: (Eq a, Integral a) => TreeT [Word8] a
shapeDeleteT = Test treeEq (Radix.shape (\_ -> Radix.empty) . Radix.feedBytes)
                           (No.shape (\_ -> No.empty))



splitLT :: Eq a => SplitT (No.Openness, [Word8]) a
splitLT = Test splitEq (\(o, k) -> Radix.splitL o $ Radix.feedBytes k) (uncurry No.splitL)

splitLookupT :: Eq a => SplitLookupT [Word8] a
splitLookupT = Test splitLookupEq (Radix.splitLookup . Radix.feedBytes) No.splitLookup



lookupLT :: Eq a => LookupT (No.Openness, [Word8]) a
lookupLT = Test lookupEq (\(o, k) -> Radix.lookupL o $ Radix.feedBytes k)
                         (uncurry No.lookupL)

adjustLT :: (Eq a, Integral a) => TreeT (No.Openness, [Word8], a) a
adjustLT  = adjustLT_ Radix.adjustL

adjustLT_
  :: (Eq a, Integral a)
  => (forall x. (x -> x) -> No.Openness -> Radix.Feed -> RadixTree x -> RadixTree x)
  -> TreeT (No.Openness, [Word8], a) a
adjustLT_ g =
  let f a = (+ a)
  in Test treeEq (\(o, k, a) -> g (f a) o $ Radix.feedBytes k)
                 (\(o, k, a) -> No.adjustL (f a) o k)

adjustLWithKeyT :: (Eq a, Integral a) => TreeT (No.Openness, [Word8], a) a
adjustLWithKeyT  = adjustLWithKeyT_ Radix.adjustLWithKey

adjustLWithKeyT_
  :: (Eq a, Integral a)
  => (forall x. (Radix.Build -> x -> x) -> No.Openness -> Radix.Feed -> RadixTree x -> RadixTree x)
  -> TreeT (No.Openness, [Word8], a) a
adjustLWithKeyT_ g =
  let f a k = (+ sum (fmap fromIntegral k)) . (+ a)
  in Test treeEq (\(o, k, a) -> g (f a . Radix.buildBytes) o $ Radix.feedBytes k)
                 (\(o, k, a) -> No.adjustLWithKey (f a) o k)

updateLAdjustT
  , updateLDeleteT
 :: (Eq a, Integral a) => TreeT (No.Openness, [Word8], a) a
updateLAdjustT = updateLT_ (\a -> Just . (+ a))
updateLDeleteT = updateLT_ (\_ _ -> Nothing)

updateLT_
  :: (Eq a, Integral a)
  => (a -> a -> Maybe a) -> TreeT (No.Openness, [Word8], a) a
updateLT_ f =
  Test treeEq (\(o, k, a) -> Radix.updateL (f a) o $ Radix.feedBytes k)
              (\(o, k, a) -> No.updateL (f a) o k)

updateLWithKeyAdjustT
  , updateLWithKeyDeleteT
 :: (Eq a, Integral a) => TreeT (No.Openness, [Word8], a) a
updateLWithKeyAdjustT = updateLWithKeyT_ (\a k -> Just . (+ sum (fmap fromIntegral k)) . (+ a))
updateLWithKeyDeleteT = updateLWithKeyT_ (\_ _ _ -> Nothing)

updateLWithKeyT_
  :: (Eq a, Integral a)
  => (a -> [Word8] -> a -> Maybe a) -> TreeT (No.Openness, [Word8], a) a
updateLWithKeyT_ f =
  Test treeEq (\(o, k, a) -> Radix.updateLWithKey (f a . Radix.buildBytes) o $ Radix.feedBytes k)
              (\(o, k, a) -> No.updateLWithKey (f a) o k)

takeLT :: Eq a => TreeT (No.Openness, [Word8]) a
takeLT = Test treeEq (\(o, k) -> Radix.takeL o $ Radix.feedBytes k)
                     (uncurry No.takeL)



lookupRT :: Eq a => LookupT (No.Openness, [Word8]) a
lookupRT = Test lookupEq (\(o, k) -> Radix.lookupR o $ Radix.feedBytes k)
                         (uncurry No.lookupR)

adjustRT :: (Eq a, Integral a) => TreeT (No.Openness, [Word8], a) a
adjustRT  = adjustRT_ Radix.adjustR

adjustRT_
  :: (Eq a, Integral a)
  => (forall x. (x -> x) -> No.Openness -> Radix.Feed -> RadixTree x -> RadixTree x)
  -> TreeT (No.Openness, [Word8], a) a
adjustRT_ g =
  let f a = (+ a)
  in Test treeEq (\(o, k, a) -> g (f a) o $ Radix.feedBytes k)
                 (\(o, k, a) -> No.adjustR (f a) o k)

adjustRWithKeyT :: (Eq a, Integral a) => TreeT (No.Openness, [Word8], a) a
adjustRWithKeyT  = adjustRWithKeyT_ Radix.adjustRWithKey

adjustRWithKeyT_
  :: (Eq a, Integral a)
  => (forall x. (Radix.Build -> x -> x) -> No.Openness -> Radix.Feed -> RadixTree x -> RadixTree x)
  -> TreeT (No.Openness, [Word8], a) a
adjustRWithKeyT_ g =
  let f a k = (+ sum (fmap fromIntegral k)) . (+ a)
  in Test treeEq (\(o, k, a) -> g (f a . Radix.buildBytes) o $ Radix.feedBytes k)
                 (\(o, k, a) -> No.adjustRWithKey (f a) o k)

updateRAdjustT
  , updateRDeleteT
 :: (Eq a, Integral a) => TreeT (No.Openness, [Word8], a) a
updateRAdjustT = updateRT_ (\a -> Just . (+ a))
updateRDeleteT = updateRT_ (\_ _ -> Nothing)

updateRT_
  :: (Eq a, Integral a)
  => (a -> a -> Maybe a) -> TreeT (No.Openness, [Word8], a) a
updateRT_ f =
  Test treeEq (\(o, k, a) -> Radix.updateR (f a) o $ Radix.feedBytes k)
              (\(o, k, a) -> No.updateR (f a) o k)

updateRWithKeyAdjustT
  , updateRWithKeyDeleteT
 :: (Eq a, Integral a) => TreeT (No.Openness, [Word8], a) a
updateRWithKeyAdjustT = updateRWithKeyT_ (\a k -> Just . (+ sum (fmap fromIntegral k)) . (+ a))
updateRWithKeyDeleteT = updateRWithKeyT_ (\_ _ _ -> Nothing)

updateRWithKeyT_
  :: (Eq a, Integral a)
  => (a -> [Word8] -> a -> Maybe a) -> TreeT (No.Openness, [Word8], a) a
updateRWithKeyT_ f =
  Test treeEq (\(o, k, a) -> Radix.updateRWithKey (f a . Radix.buildBytes) o $ Radix.feedBytes k)
              (\(o, k, a) -> No.updateRWithKey (f a) o k)

takeRT :: Eq a => TreeT (No.Openness, [Word8]) a
takeRT = Test treeEq (\(o, k) -> Radix.takeR o $ Radix.feedBytes k)
                     (uncurry No.takeR)



lookupMinT :: Eq a => IdT () a (Maybe a)
lookupMinT = Test (==) (\_ -> Radix.lookupMin) (\_ -> No.lookupMin)

lookupMinWithKeyT :: Eq a => LookupT () a
lookupMinWithKeyT =
  Test lookupEq (\_ -> Radix.lookupMinWithKey) (\_ -> No.lookupMinWithKey)

adjustMinT :: (Eq a, Integral a) => TreeT () a
adjustMinT  = adjustMinT_ Radix.adjustMin

adjustMinT_
  :: (Eq a, Integral a)
  => (forall x. (x -> x) -> RadixTree x -> RadixTree x) -> TreeT () a
adjustMinT_ f = Test treeEq (\_ -> f (+ 10000)) (\_ -> No.adjustMin (+ 10000))

adjustMinWithKeyT :: (Eq a, Integral a) => TreeT () a
adjustMinWithKeyT  = adjustMinWithKeyT_ Radix.adjustMinWithKey

adjustMinWithKeyT_
  :: (Eq a, Integral a)
  => (forall x. (Radix.Build -> x -> x) -> RadixTree x -> RadixTree x) -> TreeT () a
adjustMinWithKeyT_ g =
  let f k = (+ sum (fmap fromIntegral k))
  in Test treeEq (\_ -> g (f . Radix.buildBytes)) (\_ -> No.adjustMinWithKey f)

deleteMinT :: (Eq a, Integral a) => TreeT () a
deleteMinT = Test treeEq (\_ -> Radix.deleteMin) (\_ -> No.deleteMin)

updateMinAdjustT, updateMinDeleteT :: (Eq a, Integral a) => TreeT () a
updateMinAdjustT = updateMinT_ (Just . (+ 10000))
updateMinDeleteT = updateMinT_ (\_ -> Nothing)

updateMinT_ :: (Eq a, Integral a) => (a -> Maybe a) -> TreeT () a
updateMinT_ f = Test treeEq (\_ -> Radix.updateMin f) (\_ -> No.updateMin f)

updateMinWithKeyAdjustT, updateMinWithKeyDeleteT :: (Eq a, Integral a) => TreeT () a
updateMinWithKeyAdjustT = updateMinWithKeyT_ (\k -> Just . (+ sum (fmap fromIntegral k)))
updateMinWithKeyDeleteT = updateMinWithKeyT_ (\_ _ -> Nothing)

updateMinWithKeyT_ :: (Eq a, Integral a) => ([Word8] -> a -> Maybe a) -> TreeT () a
updateMinWithKeyT_ f =
  Test treeEq (\_ -> Radix.updateMinWithKey (f . Radix.buildBytes))
              (\_ -> No.updateMinWithKey f)

minViewT :: Eq a => MinViewT () a
minViewT = Test minViewEq (\_ -> Radix.minView) (\_ -> No.minView)



lookupMaxT :: Eq a => IdT () a (Maybe a)
lookupMaxT = Test (==) (\_ -> Radix.lookupMax) (\_ -> No.lookupMax)

lookupMaxWithKeyT :: Eq a => LookupT () a
lookupMaxWithKeyT =
  Test lookupEq (\_ -> Radix.lookupMaxWithKey) (\_ -> No.lookupMaxWithKey)

adjustMaxT :: (Eq a, Integral a) => TreeT () a
adjustMaxT  = adjustMaxT_ Radix.adjustMax

adjustMaxT_
  :: (Eq a, Integral a)
  => (forall x. (x -> x) -> RadixTree x -> RadixTree x) -> TreeT () a
adjustMaxT_ f = Test treeEq (\_ -> f (+ 10000)) (\_ -> No.adjustMax (+ 10000))

adjustMaxWithKeyT :: (Eq a, Integral a) => TreeT () a
adjustMaxWithKeyT  = adjustMaxWithKeyT_ Radix.adjustMaxWithKey

adjustMaxWithKeyT_
  :: (Eq a, Integral a)
  => (forall x. (Radix.Build -> x -> x) -> RadixTree x -> RadixTree x) -> TreeT () a
adjustMaxWithKeyT_ g =
  let f k = (+ sum (fmap fromIntegral k))
  in Test treeEq (\_ -> g (f . Radix.buildBytes)) (\_ -> No.adjustMaxWithKey f)

deleteMaxT :: (Eq a, Integral a) => TreeT () a
deleteMaxT = Test treeEq (\_ -> Radix.deleteMax) (\_ -> No.deleteMax)

updateMaxAdjustT, updateMaxDeleteT :: (Eq a, Integral a) => TreeT () a
updateMaxAdjustT = updateMaxT_ (Just . (+ 10000))
updateMaxDeleteT = updateMaxT_ (\_ -> Nothing)

updateMaxT_ :: (Eq a, Integral a) => (a -> Maybe a) -> TreeT () a
updateMaxT_ f = Test treeEq (\_ -> Radix.updateMax f) (\_ -> No.updateMax f)

updateMaxWithKeyAdjustT, updateMaxWithKeyDeleteT :: (Eq a, Integral a) => TreeT () a
updateMaxWithKeyAdjustT = updateMaxWithKeyT_ (\k -> Just . (+ sum (fmap fromIntegral k)))
updateMaxWithKeyDeleteT = updateMaxWithKeyT_ (\_ _ -> Nothing)

updateMaxWithKeyT_ :: (Eq a, Integral a) => ([Word8] -> a -> Maybe a) -> TreeT () a
updateMaxWithKeyT_ f =
  Test treeEq (\_ -> Radix.updateMaxWithKey (f . Radix.buildBytes))
              (\_ -> No.updateMaxWithKey f)

maxViewT :: Eq a => MaxViewT () a
maxViewT = Test maxViewEq (\_ -> Radix.maxView) (\_ -> No.maxView)



filterT :: (Eq a, Integral a) => TreeT () a
filterT = Test treeEq (\_ -> Radix.filter odd) (\_ -> No.filter odd)

filterWithKeyT :: (Eq a, Integral a) => TreeT () a
filterWithKeyT =
  let f k a = odd $ sum (fmap fromIntegral k) + a
  in Test treeEq (\_ -> Radix.filterWithKey (f . Radix.buildBytes))
                 (\_ -> No.filterWithKey f)

mapMaybeT :: (Eq a, Integral a) => TreeT () a
mapMaybeT =
  let f a | odd a     = Nothing
          | otherwise = Just a

  in Test treeEq (\_ -> Radix.mapMaybe f) (\_ -> No.mapMaybe f)

mapMaybeWithKeyT :: (Eq a, Integral a) => TreeT () a
mapMaybeWithKeyT =
  let f k a | odd (sum (fmap fromIntegral k) + a) = Nothing
            | otherwise                           = Just a

  in Test treeEq (\_ -> Radix.mapMaybeWithKey (f . Radix.buildBytes))
                 (\_ -> No.mapMaybeWithKey f)

partitionT :: (Eq a, Integral a) => SplitT () a
partitionT = Test splitEq (\_ -> Radix.partition odd) (\_ -> No.partition odd)

partitionWithKeyT :: (Eq a, Integral a) => SplitT () a
partitionWithKeyT =
  let f k a = odd $ sum (fmap fromIntegral k) + a
  in Test splitEq (\_ -> Radix.partitionWithKey (f . Radix.buildBytes))
                  (\_ -> No.partitionWithKey f)

mapEitherT :: (Eq a, Integral a) => SplitT () a
mapEitherT =
  let f a | odd a     = Left a
          | otherwise = Right a

  in Test splitEq (\_ -> Radix.mapEither f) (\_ -> No.mapEither f)

mapEitherWithKeyT :: (Eq a, Integral a) => SplitT () a
mapEitherWithKeyT =
  let f k a | odd (sum (fmap fromIntegral k) + a) = Left a
            | otherwise                = Right a

  in Test splitEq (\_ -> Radix.mapEitherWithKey (f . Radix.buildBytes))
                  (\_ -> No.mapEitherWithKey f)



mapT :: (Eq a, Num a) => TreeT () a
mapT  = mapT_ Radix.map

mapT_ :: (Eq a, Num a) => (forall x. (x -> x) -> RadixTree x -> RadixTree x) -> TreeT () a
mapT_ g =
  let f = (+ 10000)
  in Test treeEq (\_ -> g f) (\_ -> No.map f)

mapWithKeyT :: (Eq a, Num a) => TreeT () a
mapWithKeyT  = mapWithKeyT_ Radix.mapWithKey

mapWithKeyT_
  :: (Eq a, Num a)
  => (forall x. (Radix.Build -> x -> x) -> RadixTree x -> RadixTree x) -> TreeT () a
mapWithKeyT_ g =
  let f k = (+ sum (fmap fromIntegral k)) . (+ 10000)
  in Test treeEq (\_ -> g (f . Radix.buildBytes)) (\_ -> No.mapWithKey f)


foldlT, foldlT' :: (Eq a, Num a) => IdT () a [a]
foldlT  = foldlT_ Radix.foldl
foldlT' = foldlT_ Radix.foldl'

foldlT_ :: Eq a => (forall x. (x -> a -> x) -> x -> RadixTree a -> x) -> IdT () a [a]
foldlT_ g =
  Test (==) (\_ -> g (flip (:)) []) (\_ -> No.foldl (flip (:)) [])

foldlWithKeyT, foldlWithKeyT' :: Eq a => IdT () a [([Word8], a)]
foldlWithKeyT  = foldlWithKeyT_ Radix.foldlWithKey
foldlWithKeyT' = foldlWithKeyT_ Radix.foldlWithKey'

foldlWithKeyT_
  :: Eq a
  => (forall x. (x -> Radix.Build -> a -> x) -> x -> RadixTree a -> x)
  -> IdT () a [([Word8], a)]
foldlWithKeyT_ g =
  Test (==) (\_ -> g (\z k a -> (Radix.buildBytes k, a) : z) [])
            (\_ -> No.foldlWithKey (\z k a -> (k, a) : z) [])



foldrT, foldrT' :: Eq a => IdT () a [a]
foldrT  = foldrT_ Radix.foldr
foldrT' = foldrT_ Radix.foldr'

foldrT_ :: Eq a => (forall x. (a -> x -> x) -> x -> RadixTree a -> x) -> IdT () a [a]
foldrT_ g = Test (==) (\_ -> g (:) []) (\_ -> No.foldr (:) [])

foldrWithKeyT, foldrWithKeyT' :: (Eq a, Num a) => IdT () a [([Word8], a)]
foldrWithKeyT  = foldrWithKeyT_ Radix.foldrWithKey
foldrWithKeyT' = foldrWithKeyT_ Radix.foldrWithKey'

foldrWithKeyT_
  :: (Eq a, Num a)
  => (forall y. (Radix.Build -> a -> y -> y) -> y -> RadixTree a -> y)
  -> IdT () a [([Word8], a)]
foldrWithKeyT_ g = Test (==) (\_ -> g (\k a -> (:) (Radix.buildBytes k, a)) [])
                             (\_ -> No.foldrWithKey (\k a -> (:) (k, a)) [])



foldMapT :: Eq a => IdT () a [a]
foldMapT = Test (==) (\_ -> Radix.foldMap (:[])) (\_ -> No.foldMap (:[]))

foldMapWithKeyT :: Eq a => IdT () a [([Word8], a)]
foldMapWithKeyT =
  Test (==) (\_ -> Radix.foldMapWithKey (\k a -> [(Radix.buildBytes k, a)]))
            (\_ -> No.foldMapWithKey (\k a -> [(k, a)]))



idTreeEq :: Eq a => Identity (RadixTree a) -> Identity (NoTree [Word8] a) -> Bool
idTreeEq (Identity a) (Identity b) = treeEq a b

traverseT
  :: (Eq a, Num a)
  => Test s (RadixTree a) (NoTree [Word8] a)
            (Identity (RadixTree a)) (Identity (NoTree [Word8] a))
traverseT =
  let f = Identity . (+ 10000)
  in Test idTreeEq (\_ -> Radix.traverse f) (\_ -> No.traverse f)

traverseWithKeyT
  :: (Eq a, Num a)
  => Test s (RadixTree a) (NoTree [Word8] a)
            (Identity (RadixTree a)) (Identity (NoTree [Word8] a))
traverseWithKeyT =
  let f k a = Identity $ sum (fmap fromIntegral k) + 10000 + a
  in Test idTreeEq (\_ -> Radix.traverseWithKey (f . Radix.buildBytes))
                   (\_ -> No.traverseWithKey f)



unionT :: Eq a => TreeT (RadixTree a, NoTree [Word8] a) a
unionT = Test treeEq (Radix.union . fst) (No.unionL . snd)

unionLT :: Eq a => TreeT (RadixTree a, NoTree [Word8] a) a
unionLT = Test treeEq (Radix.unionL . fst) (No.unionL . snd)

unionWithT :: Eq a => TreeT (RadixTree a, NoTree [Word8] a) a
unionWithT = Test treeEq (Radix.unionWith (\_ y -> y) . fst)
                         (No.unionWithKey (\_ _ y -> y) . snd)

unionWithKeyT, mergeUnionT :: Eq a => TreeT (RadixTree a, NoTree [Word8] a) a
unionWithKeyT = unionWithKeyT_ Radix.unionWithKey
mergeUnionT   =
  unionWithKeyT_ $ \f ->
    Radix.merge (\k a b -> Just $! f k a b)
      (\_ -> Just) (\_ -> id) (\_ -> Just) (\_ -> id)

unionWithKeyT_
  :: Eq a
  => ((Radix.Build -> a -> a -> a) -> RadixTree a -> RadixTree a -> RadixTree a)
  -> TreeT (RadixTree a, NoTree [Word8] a) a
unionWithKeyT_ g =
  let f k a b | odd $ sum (fmap (fromIntegral :: Word8 -> Int) k) = a
              | otherwise                                         = b

  in Test treeEq (g (f . Radix.buildBytes) . fst)
                 (No.unionWithKey f . snd)



differenceT :: Eq a => TreeT (RadixTree a, NoTree [Word8] a) a
differenceT = Test treeEq (Radix.difference . fst) (No.difference . snd)

differenceWithT :: Eq a => TreeT (RadixTree a, NoTree [Word8] a) a
differenceWithT = Test treeEq (Radix.differenceWith (\_ -> Just) . fst)
                              (No.differenceWithKey (\_ _ -> Just) . snd)

differenceWithKeyT
  , mergeDifferenceT
 :: (Eq a, Integral a) => TreeT (RadixTree a, NoTree [Word8] a) a
differenceWithKeyT = differenceWithKeyT_ Radix.differenceWithKey
mergeDifferenceT    =
  differenceWithKeyT_ $ \f ->
    Radix.merge f (\_ -> Just) (\_ -> id) (\_ _ -> Nothing) (\_ _ -> Radix1.empty)

differenceWithKeyT_
  :: (Eq a, Integral a)
  => ((Radix.Build -> a -> a -> Maybe a) -> RadixTree a -> RadixTree a -> RadixTree a)
  -> TreeT (RadixTree a, NoTree [Word8] a) a
differenceWithKeyT_ g =
  let f k a b | odd $ sum (fmap (fromIntegral :: Word8 -> Int) k) = Just a
              | otherwise                                         = if even b
                                                                      then Just b
                                                                      else Nothing
  in Test treeEq (g (f . Radix.buildBytes) . fst)
                 (No.differenceWithKey f . snd)



disjointT :: Eq a => IdT (RadixTree a, NoTree [Word8] a) a Bool
disjointT = Test (==) (Radix.disjoint . fst) (\(_, a) -> No.null . No.intersectionL a)

intersectionT :: Eq a => TreeT (RadixTree a, NoTree [Word8] a) a
intersectionT = Test treeEq (Radix.intersection . fst) (No.intersectionL . snd)

intersectionLT :: Eq a => TreeT (RadixTree a, NoTree [Word8] a) a
intersectionLT = Test treeEq (Radix.intersectionL . fst) (No.intersectionL . snd)

intersectionWithT :: Eq a => TreeT (RadixTree a, NoTree [Word8] a) a
intersectionWithT = Test treeEq (Radix.intersectionWith (\_ y -> y) . fst)
                                (No.intersectionWithKey (\_ _ y -> y) . snd)

intersectionWithKeyT, mergeIntersectionT :: Eq a => TreeT (RadixTree a, NoTree [Word8] a) a
intersectionWithKeyT = intersectionWithKeyT_ Radix.intersectionWithKey
mergeIntersectionT   =
  intersectionWithKeyT_ $ \f ->
    Radix.merge (\k a b -> Just $! f k a b)
      (\_ _ -> Nothing) (\_ _ -> Radix1.empty) (\_ _ -> Nothing) (\_ _ -> Radix1.empty)

intersectionWithKeyT_
  :: Eq a
  => ((Radix.Build -> a -> a -> a) -> RadixTree a -> RadixTree a -> RadixTree a)
  -> TreeT (RadixTree a, NoTree [Word8] a) a
intersectionWithKeyT_ g =
  let f k a b | odd $ sum (fmap (fromIntegral :: Word8 -> Int) k) = a
              | otherwise                                         = b

  in Test treeEq (g (f . Radix.buildBytes) . fst)
                 (No.intersectionWithKey f . snd)



compareT :: Eq a => IdT (RadixTree a, NoTree [Word8] a) a Radix.PartialOrdering
compareT = Test (==) (Radix.compare (==) . fst) (No.compare . snd)



test :: Spec
test = do
  describe "Single-key" $ do
    it "lookup"           $ run unary1_ lookupT
    it "find"             $ run unary1  findT
    it "member"           $ run unary1_ memberT
    it "subtree"          $ run unary1_ subtreeT
    it "move/single"      $ run unary1_ moveSingleT
    it "move/thirds"      $ run unary1_ moveThirdsT
    it "insert"           $ run unary1  insertT
    it "insertWith"       $ run unary1  insertWithT
    it "adjust"           $ run unary1  adjustT
    it "delete"           $ run unary1_ deleteT
    it "prune"            $ run unary1R pruneT
    it "update/adjust"    $ run unary1  updateAdjustT
    it "update/delete"    $ run unary1  updateDeleteT
    it "alter/insert"     $ run unary1  alterInsertT
    it "alter/insertWith" $ run unary1  alterInsertWithT
    it "alter/adjust"     $ run unary1  alterAdjustT
    it "alter/delete"     $ run unary1  alterDeleteT
    it "shape/insert"     $ run unary1_ shapeInsertT
    it "shape/adjust"     $ run unary1_ shapeAdjustT
    it "shape/filter"     $ run unary1_ shapeFilterT
    it "shape/delete"     $ run unary1_ shapeDeleteT

  describe "Split" $ do
    it "splitL"           $ run unary1R splitLT
    it "splitLookup"      $ run unary1_ splitLookupT

  describe "Left" $ do
    it "lookupL"               $ run unary1R lookupLT
    it "adjustL"               $ run unary1F adjustLT
    it "adjustLWithKey"        $ run unary1F adjustLWithKeyT
    it "updateL/adjust"        $ run unary1F updateLAdjustT
    it "updateL/delete"        $ run unary1F updateLDeleteT
    it "updateLWithKey/adjust" $ run unary1F updateLWithKeyAdjustT
    it "updateLWithKey/delete" $ run unary1F updateLWithKeyDeleteT
    it "takeL"                 $ run unary1R takeLT

  describe "Right" $ do
    it "lookupR"               $ run unary1R lookupRT
    it "adjustR"               $ run unary1F adjustRT
    it "adjustRWithKey"        $ run unary1F adjustRWithKeyT
    it "updateR/adjust"        $ run unary1F updateRAdjustT
    it "updateR/delete"        $ run unary1F updateRDeleteT
    it "updateRWithKey/adjust" $ run unary1F updateRWithKeyAdjustT
    it "updateRWithKey/delete" $ run unary1F updateRWithKeyDeleteT
    it "takeR"                 $ run unary1R takeRT

  describe "Min" $ do
    it "lookupMin"               $ run unary0 lookupMinT
    it "lookupMinWithKey"        $ run unary0 lookupMinWithKeyT
    it "adjustMin"               $ run unary0 adjustMinT
    it "adjustMinWithKey"        $ run unary0 adjustMinWithKeyT
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
    it "prefix"          $ run unary1_ prefixT
    it "map"             $ run unary0  mapT
    it "mapWithKey"      $ run unary0  mapWithKeyT
    it "foldl"           $ run unary0  foldlT
    it "foldl'"          $ run unary0  foldlT'
    it "foldlWithKey"    $ run unary0  foldlWithKeyT
    it "foldlWithKey'"   $ run unary0  foldlWithKeyT'
    it "foldr"           $ run unary0  foldrT
    it "foldr'"          $ run unary0  foldrT'
    it "foldrWithKey"    $ run unary0  foldrWithKeyT
    it "foldrWithKey'"   $ run unary0  foldrWithKeyT'
    it "foldMap"         $ run unary0  foldMapT
    it "foldMapWithKey"  $ run unary0  foldMapWithKeyT
    it "traverse"        $ run unary0  traverseT
    it "traverseWithKey" $ run unary0  traverseWithKeyT

  describe "Merge" $ do
    it "union"                $ run binary  unionT
    it "unionL"               $ run binaryL unionLT
    it "unionWith"            $ run binaryL unionWithT
    it "unionWithKey"         $ run binaryL unionWithKeyT
    it "difference"           $ run binaryL differenceT
    it "differenceWith"       $ run binaryL differenceWithT
    it "differenceWithKey"    $ run binaryL differenceWithKeyT
    it "disjoint/yes"         $ run binary  disjointT
    it "disjoint/no"          $ run binaryL disjointT
    it "intersection"         $ run binary  intersectionT
    it "intersectionL"        $ run binaryL intersectionLT
    it "intersectionWith"     $ run binaryL intersectionWithT
    it "intersectionWithKey"  $ run binaryL intersectionWithKeyT
    it "compare/subset"       $ run subset   compareT
    it "compare/superset"     $ run superset compareT
    it "compare/equal"        $ run equal    compareT
    it "compare/incomparable" $ run binary   compareT
    it "merge/union"          $ run binaryL mergeUnionT
    it "merge/difference"     $ run binaryL mergeDifferenceT
    it "merge/intersection"   $ run binaryL mergeIntersectionT
