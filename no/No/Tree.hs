{-# LANGUAGE DerivingStrategies
           , GeneralizedNewtypeDeriving
           , PatternSynonyms #-}

module No.Tree
  ( NoTree
  , empty
  , singleton

  , No.Tree.null

  , fromList
  , No.Tree.toList

  , No.Tree.map
  , mapWithKey

  , size
  , No.Tree.foldl
  , foldlWithKey
  , No.Tree.foldr
  , foldrWithKey
  , No.Tree.foldMap
  , foldMapWithKey

  , No.Tree.traverse
  , traverseWithKey

  , No.Tree.lookup
  , find
  , member

  , prefix
  , subtree

  , insert
  , insertWith
  , adjust
  , delete
  , update
  , alter

  , prune
  , shape

  , Openness (..)
  , lookupL
  , adjustL
  , adjustLWithKey
  , deleteL
  , updateL
  , updateLWithKey
  , takeL

  , lookupR
  , adjustR
  , adjustRWithKey
  , deleteR
  , updateR
  , updateRWithKey
  , takeR

  , Range (WordRange, StringRange, ..)
  , adjustRange
  , adjustRangeWithKey
  , deleteRange
  , updateRange
  , updateRangeWithKey
  , takeRange

  , unionL
  , unionWithKey

  , difference
  , differenceWithKey

  , intersectionL
  , intersectionWithKey

  , No.Tree.compare

  , splitL
  , splitR
  , splitLookup

  , No.Tree.filter
  , filterWithKey

  , No.Tree.mapMaybe
  , mapMaybeWithKey

  , partition
  , partitionWithKey

  , mapEither
  , mapEitherWithKey

  , lookupMin
  , lookupMinWithKey
  , lookupMax
  , lookupMaxWithKey

  , adjustMin
  , adjustMinWithKey
  , adjustMax
  , adjustMaxWithKey

  , deleteMin
  , deleteMax

  , updateMin
  , updateMinWithKey
  , updateMax
  , updateMaxWithKey

  , minView
  , maxView
  ) where

import           Data.Patricia.Word.Strict (PartialOrdering (..))
import           Data.RadixTree.Word8.Strict (Openness (..))

import           Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import           Data.Maybe
import           Data.Either
import           Data.Foldable (toList)
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty



newtype NoTree k a = NoTree { getNoTree :: Seq (k, a) }
                     deriving newtype (Show, Eq)

empty :: NoTree k a
empty = NoTree Seq.empty

singleton :: k -> a -> NoTree k a
singleton k a = NoTree $ Seq.singleton (k, a)


null :: NoTree k a -> Bool
null = Seq.null . getNoTree



fromList :: Ord k => [(k, a)] -> NoTree k a
fromList = NoTree . Seq.fromList . List.nubBy (\(k, _) (l, _) -> k == l) . List.sortOn fst

toList :: NoTree k a -> [(k, a)]
toList (NoTree as) = Data.Foldable.toList as



map :: (a -> b) -> NoTree k a -> NoTree k b
map f = mapWithKey (\_ -> f)

mapWithKey :: (k -> a -> b) -> NoTree k a -> NoTree k b
mapWithKey f (NoTree as) = NoTree $ fmap (\(ks, a) -> (ks, f ks a)) as


size :: NoTree k a -> Int
size = No.Tree.foldr (\_ -> (+) 1) 0

foldl :: (b -> a -> b) -> b -> NoTree k a -> b
foldl f = foldlWithKey (\z _ -> f z)

foldlWithKey :: (b -> k -> a -> b) -> b -> NoTree k a -> b
foldlWithKey f z (NoTree as) = Prelude.foldl (\z' (ks, a) -> f z' ks a) z as

foldr :: (a -> b -> b) -> b -> NoTree k a -> b
foldr f = foldrWithKey (\_ -> f)

foldrWithKey :: (k -> a -> b -> b) -> b -> NoTree k a -> b
foldrWithKey f z (NoTree as) = Prelude.foldr (\(ks, a) -> f ks a) z as

foldMap :: Monoid m => (a -> m) -> NoTree k a -> m
foldMap f = foldMapWithKey (\_ -> f)

foldMapWithKey :: Monoid m => (k -> a -> m) -> NoTree k a -> m
foldMapWithKey f (NoTree as) = Prelude.foldMap (\(ks, a) -> f ks a) as

traverse :: Applicative f => (a -> f b) -> NoTree k a -> f (NoTree k b)
traverse f = traverseWithKey (\_ -> f)

traverseWithKey
  :: Applicative f => (k -> a -> f b) -> NoTree k a -> f (NoTree k b)
traverseWithKey f (NoTree as) =
  NoTree <$> Prelude.traverse (\(ks, a) -> (,) ks <$> f ks a) as



lookup :: Ord k => k -> NoTree k a -> Maybe a
lookup k = (\(_, mx, _) -> mx) <$> splitLookup k

find :: Ord k => a -> k -> NoTree k a -> a
find d k = (\(_, mx, _) -> fromMaybe d mx) <$> splitLookup k

member :: Ord k => k -> NoTree k a -> Bool
member k = (\(_, mx, _) -> maybe False (\_ -> True) mx) <$> splitLookup k



subtree :: Ord k => [k] -> NoTree [k] a -> NoTree [k] a
subtree ks (NoTree as) =
  let (_, bs) = Seq.spanl (\(w, _) -> not $ List.isPrefixOf ks w) as
      (cs, _) = Seq.spanl (\(w, _) ->       List.isPrefixOf ks w) bs

  in NoTree $ fmap (\(k, a) -> (drop (length ks) k, a)) cs

prefix :: [k] -> NoTree [k] a -> NoTree [k] a
prefix k (NoTree as) = NoTree $ fmap (\(w, a) -> (k <> w, a)) as



insert :: Ord k => k -> a -> NoTree k a -> NoTree k a
insert k a = alter (\_ -> Just a) k

insertWith :: Ord k => (a -> a) -> k -> a -> NoTree k a -> NoTree k a
insertWith f k a = alter (Just . maybe a f) k

adjust :: Ord k => (a -> a) -> k -> NoTree k a -> NoTree k a
adjust f = alter (fmap f)

delete :: Ord k => k -> NoTree k a -> NoTree k a
delete k = alter (\_ -> Nothing) k

update :: Ord k => (a -> Maybe a) -> k -> NoTree k a -> NoTree k a
update f k = alter (f =<<) k

alter :: Ord k => (Maybe a -> Maybe a) -> k -> NoTree k a -> NoTree k a
alter f k no =
  let ~(NoTree as, mx, NoTree bs) = splitLookup k no
  in case f mx of
       Just y  -> NoTree $ as <> ((k, y) :<| bs)
       Nothing -> NoTree $ as <> bs



prune :: Ord k => Openness -> [k] -> NoTree [k] a -> NoTree [k] a
prune o ks xs =
  let (NoTree ls, NoTree ms, NoTree rs) = breakOnPrefix ks xs

  in NoTree $ ls <> case ms of
                      (x, y) :<| _ | x == ks, Open <- o -> (x, y) :<| rs
                      _                                 -> rs

shape :: Ord k => (NoTree [k] a -> NoTree [k] a) -> [k] -> NoTree [k] a -> NoTree [k] a
shape f ks xs =
  let (NoTree ls, NoTree ms, NoTree rs) = breakOnPrefix ks xs

      NoTree ms' = f . NoTree $ fmap (\(k, a) -> (drop (length ks) k, a)) ms

  in NoTree $ ls <> fmap (\(k, a) -> (ks <> k, a)) ms' <> rs

breakOnPrefix :: Ord k => [k] -> NoTree [k] a -> (NoTree [k] a, NoTree [k] a, NoTree [k] a)
breakOnPrefix ks (NoTree xs) =
  let ~(as, bs) = Seq.spanl (\(ws, _) -> take (length ks) ws < ks) xs
      ~(cs, ds) = Seq.spanl (\(ws, _) -> List.isPrefixOf ks ws) bs

  in (NoTree as, NoTree cs, NoTree ds)



lookupL :: Ord k => Openness -> k -> NoTree k a -> Maybe (k, a)
lookupL o k no =
  let NoTree as = takeL o k no
  in case as of
       _ :|> ka  -> Just ka
       Seq.Empty -> Nothing

adjustL :: Ord k => (a -> a) -> Openness -> k -> NoTree k a -> NoTree k a
adjustL f = shapeL (No.Tree.map f)

adjustLWithKey :: Ord k => (k -> a -> a) -> Openness -> k -> NoTree k a -> NoTree k a
adjustLWithKey f = shapeL (mapWithKey f)

deleteL :: Ord k => Openness -> k -> NoTree k a -> NoTree k a
deleteL = shapeL (\_ -> empty)

updateL :: Ord k => (a -> Maybe a) -> Openness -> k -> NoTree k a -> NoTree k a
updateL f = shapeL (No.Tree.mapMaybe f)

updateLWithKey :: Ord k => (k -> a -> Maybe a) -> Openness -> k -> NoTree k a -> NoTree k a
updateLWithKey f = shapeL (mapMaybeWithKey f)

takeL :: Ord k => Openness -> k -> NoTree k a -> NoTree k a
takeL Closed = deleteR Open
takeL Open   = deleteR Closed

shapeL :: Ord k => (NoTree k a -> NoTree k a) -> Openness -> k -> NoTree k a -> NoTree k a
shapeL f o k no =
  let ~(NoTree as, mx, NoTree bs) = splitLookup k no
  in case mx of
       Nothing -> NoTree $ getNoTree (f $ NoTree as) <> bs
       Just x  ->
         case o of
           Closed -> NoTree $ getNoTree (f $ NoTree (as :|> (k, x))) <> bs
           Open   -> NoTree $ getNoTree (f $ NoTree as) <> ((k, x) :<| bs)



lookupR :: Ord k => Openness -> k -> NoTree k a -> Maybe (k, a)
lookupR o k no =
  let NoTree as = takeR o k no
  in case as of
       ka :<| _  -> Just ka
       Seq.Empty -> Nothing

adjustR :: Ord k => (a -> a) -> Openness -> k -> NoTree k a -> NoTree k a
adjustR f = shapeR (No.Tree.map f)

adjustRWithKey :: Ord k => (k -> a -> a) -> Openness -> k -> NoTree k a -> NoTree k a
adjustRWithKey f = shapeR (mapWithKey f)

deleteR :: Ord k => Openness -> k -> NoTree k a -> NoTree k a
deleteR = shapeR (\_ -> empty)

updateR :: Ord k => (a -> Maybe a) -> Openness -> k -> NoTree k a -> NoTree k a
updateR f = shapeR (No.Tree.mapMaybe f)

updateRWithKey :: Ord k => (k -> a -> Maybe a) -> Openness -> k -> NoTree k a -> NoTree k a
updateRWithKey f = shapeR (mapMaybeWithKey f)

takeR :: Ord k => Openness -> k -> NoTree k a -> NoTree k a
takeR Closed = deleteL Open
takeR Open   = deleteL Closed

shapeR :: Ord k => (NoTree k a -> NoTree k a) -> Openness -> k -> NoTree k a -> NoTree k a
shapeR f o k no =
  let ~(NoTree as, mx, NoTree bs) = splitLookup k no
  in case mx of
       Nothing -> NoTree $ as <> getNoTree (f $ NoTree bs)
       Just x  ->
         case o of
           Closed -> NoTree $ as <> getNoTree (f . NoTree $ (k, x) :<| bs)
           Open   -> NoTree $ (as :|> (k, x)) <> getNoTree (f $ NoTree bs)



data Range k = UnsafeRange
                 {-# UNPACK #-} !Openness
                 k
                 {-# UNPACK #-} !Openness
                 k

instance Show k => Show (Range k) where
  showsPrec d (UnsafeRange oL kL oR kR) =
    showParen (d > 10) $
      showString "Range " . shows oL
              . showChar ' ' . shows kL
              . showChar ' ' . shows oR
              . showChar ' ' . shows kR

pattern WordRange
  :: (Bounded k, Num k, Ord k)
  => Openness
  -> k
  -> Openness
  -> k
  -> Range k
pattern WordRange oL kL oR kR <- UnsafeRange oL kL oR kR
  where
    WordRange o1 k1 o2 k2 =
      case Prelude.compare k1 k2 of
        LT -> UnsafeRange o1 k1 o2 k2
        GT -> UnsafeRange o2 k2 o1 k1
        EQ ->
          let o | Closed <- o1, Closed <- o2 = Closed
                | otherwise                  = Open

          in if k1 == maxBound
               then UnsafeRange Open (maxBound - 1) o maxBound
               else UnsafeRange o k1 Open (k1 + 1)

pattern StringRange
  :: (Bounded k, Ord k, Num k)
  => Openness
  -> NonEmpty k
  -> Openness
  -> NonEmpty k
  -> Range (NonEmpty k)
pattern StringRange oL kL oR kR <- UnsafeRange oL kL oR kR
  where
    StringRange o1 k1 o2 k2 =
      case Prelude.compare k1 k2 of
        LT -> UnsafeRange o1 k1 o2 k2
        GT -> UnsafeRange o2 k2 o1 k1
        EQ ->
          let o | Closed <- o1, Closed <- o2 = Closed
                | otherwise                  = Open

              x = NonEmpty.last k1
              xs = NonEmpty.init k1

          in if x == maxBound
               then UnsafeRange Open (NonEmpty.fromList $ xs <> [x - 1]) o k1
               else UnsafeRange o k1 Open (NonEmpty.fromList $ xs <> [x + 1])



adjustRange :: Ord k => (a -> a) -> Range k -> NoTree k a -> NoTree k a
adjustRange f = shapeRange (No.Tree.map f)

adjustRangeWithKey :: Ord k => (k -> a -> a) -> Range k -> NoTree k a -> NoTree k a
adjustRangeWithKey f = shapeRange (mapWithKey f)

deleteRange :: Ord k => Range k -> NoTree k a -> NoTree k a
deleteRange = shapeRange (\_ -> empty)

updateRange :: Ord k => (a -> Maybe a) -> Range k -> NoTree k a -> NoTree k a
updateRange f = shapeRange (No.Tree.mapMaybe f)

updateRangeWithKey :: Ord k => (k -> a -> Maybe a) -> Range k -> NoTree k a -> NoTree k a
updateRangeWithKey f = shapeRange (mapMaybeWithKey f)

takeRange :: Ord k => Range k -> NoTree k a -> NoTree k a
takeRange (UnsafeRange oL kL oR kR) = takeR oL kL . takeL oR kR

shapeRange :: Ord k => (NoTree k a -> NoTree k a) -> Range k -> NoTree k a -> NoTree k a
shapeRange f (UnsafeRange oL kL oR kR) = shapeR (shapeL f oR kR) oL kL



merge
  :: Ord k
  => (k -> a -> b -> Maybe c)
  -> (a -> Maybe c)
  -> (b -> Maybe c)
  -> NoTree k a
  -> NoTree k b
  -> NoTree k c
merge f l r (NoTree as) (NoTree bs) =
  NoTree . Seq.fromList $ go (Data.Foldable.toList as) (Data.Foldable.toList bs)
  where
    go ((ks, x) : xs) ((ls, y) : ys) =
      case Prelude.compare ks ls of
        LT -> let rest = go xs ((ls, y) : ys)
              in case l x of
                   Just z  -> (ks, z) : rest
                   Nothing -> rest

        EQ -> let rest = go xs ys
              in case f ks x y of
                   Just z  -> (ks, z) : rest
                   Nothing -> rest

        GT -> let rest = go ((ks, x) : xs) ys
              in case r y of
                   Just z  -> (ls, z) : rest
                   Nothing -> rest

    go xs [] = Data.Maybe.mapMaybe (\(ks, x) -> (,) ks <$> l x) xs
    go [] ys = Data.Maybe.mapMaybe (\(ls, y) -> (,) ls <$> r y) ys



unionL :: Ord k => NoTree k a -> NoTree k a -> NoTree k a
unionL = unionWithKey (\_ a _ -> a)

unionWithKey
  :: Ord k => (k -> a -> a -> a) -> NoTree k a -> NoTree k a -> NoTree k a
unionWithKey f = merge (\ks a b -> Just $ f ks a b) Just Just


difference :: Ord k => NoTree k a -> NoTree k b -> NoTree k a
difference = differenceWithKey (\_ _ _ -> Nothing)

differenceWithKey
  :: Ord k => (k -> a -> b -> Maybe a) -> NoTree k a -> NoTree k b -> NoTree k a
differenceWithKey f = merge f Just (\_ -> Nothing)


intersectionL :: Ord k => NoTree k a -> NoTree k b -> NoTree k a
intersectionL = intersectionWithKey (\_ a _ -> a)

intersectionWithKey
  :: Ord k => (k -> a -> b -> c) -> NoTree k a -> NoTree k b -> NoTree k c
intersectionWithKey f =
  merge (\k a b -> Just $ f k a b) (\_ -> Nothing) (\_ -> Nothing)



compare :: (Eq a, Ord k) => NoTree k a -> NoTree k a -> PartialOrdering
compare xs@(NoTree as) ys@(NoTree bs)
  | as == bs                                   = Equal

  | NoTree is <- intersectionL xs ys, is == as
  , NoTree us <- unionL        xs ys, us == bs = Subset

  | NoTree is <- intersectionL xs ys, is == bs
  , NoTree us <- unionL        xs ys, us == as = Superset

  | otherwise                                  = Incomparable



splitL :: Ord k => Openness -> k -> NoTree k a -> (NoTree k a, NoTree k a)
splitL o k t =
  let (NoTree l, mx, NoTree r) = splitLookup k t
  in case mx of
       Just x  -> case o of
                    Closed -> (NoTree $ l :|> (k, x), NoTree r)
                    Open   -> (NoTree $ l, NoTree $ (k, x) :<| r)

       Nothing -> (NoTree l, NoTree r)

splitR :: Ord k => k -> NoTree k a -> (NoTree k a, NoTree k a)
splitR k t =
  let (l, mx, NoTree r) = splitLookup k t
  in ( l
     , NoTree $ case mx of
                  Just x  -> (k, x) :<| r
                  Nothing -> r
     )

splitLookup :: Ord k => k -> NoTree k a -> (NoTree k a, Maybe a, NoTree k a)
splitLookup ws (NoTree as) =
  let (before, after) = Seq.spanl (\(ks, _) -> ks < ws) as
  in case after of
       (cs, a) :<| rest | cs == ws -> (NoTree before, Just a , NoTree rest)
       _                           -> (NoTree before, Nothing, NoTree after)



filter :: (a -> Bool) -> NoTree k a -> NoTree k a
filter f = fst . partition f

filterWithKey :: (k -> a -> Bool) -> NoTree k a -> NoTree k a
filterWithKey f = fst . partitionWithKey f

mapMaybe :: (a -> Maybe b) -> NoTree k a -> NoTree k b
mapMaybe f = fst . mapEitherWithKey (\_ -> maybe (Right ()) Left . f)

mapMaybeWithKey :: (k -> a -> Maybe b) -> NoTree k a -> NoTree k b
mapMaybeWithKey f = fst . mapEitherWithKey (\ks -> maybe (Right ()) Left . f ks)

partition :: (a -> Bool) -> NoTree k a -> (NoTree k a, NoTree k a)
partition f = mapEitherWithKey (\_ a -> if f a then Left a else Right a)

partitionWithKey :: (k -> a -> Bool) -> NoTree k a -> (NoTree k a, NoTree k a)
partitionWithKey f = mapEitherWithKey (\ks a -> if f ks a then Left a else Right a)

mapEither :: (a -> Either b c) -> NoTree k a -> (NoTree k b, NoTree k c)
mapEither f = mapEitherWithKey (\_ -> f)

mapEitherWithKey
  :: (k -> a -> Either b c) -> NoTree k a -> (NoTree k b, NoTree k c)
mapEitherWithKey f (NoTree as) =
  let ~(bs, cs) = partitionEithers $
                    flip fmap (Data.Foldable.toList as) $ \(ks, a) ->
                      case f ks a of
                        Left b  -> Left (ks, b)
                        Right c -> Right (ks, c)

  in (NoTree $ Seq.fromList bs, NoTree $ Seq.fromList cs)



lookupMin :: NoTree k a -> Maybe a
lookupMin t = (\ (_, a, _) -> a) <$> minView t

lookupMinWithKey :: NoTree k a -> Maybe (k, a)
lookupMinWithKey t = (\ (k, a, _) -> (k, a)) <$> minView t

deleteMin :: NoTree k a -> NoTree k a
deleteMin = updateMin (\_ -> Nothing)

adjustMin :: (a -> a) -> NoTree k a -> NoTree k a
adjustMin f = adjustMinWithKey (\_ -> f)

adjustMinWithKey :: (k -> a -> a) -> NoTree k a -> NoTree k a
adjustMinWithKey f = updateMinWithKey (\k a -> Just $ f k a)

updateMin :: (a -> Maybe a) -> NoTree k a -> NoTree k a
updateMin f = updateMinWithKey (\_ -> f)

updateMinWithKey :: (k -> a -> Maybe a) -> NoTree k a -> NoTree k a
updateMinWithKey f (NoTree as) =
  NoTree $
    case as of
      (k, a) :<| bs ->
        case f k a of
          Just b  -> (k, b) :<| bs
          Nothing -> bs

      Empty         -> Seq.empty

minView :: NoTree k a -> Maybe (k, a, NoTree k a)
minView (NoTree as) =
  case as of
    (k, a) :<| bs -> Just (k, a, NoTree bs)
    Empty         -> Nothing



lookupMax :: NoTree k a -> Maybe a
lookupMax t = (\ (_, _, a) -> a) <$> maxView t

lookupMaxWithKey :: NoTree k a -> Maybe (k, a)
lookupMaxWithKey t = (\ (_, k, a) -> (k, a)) <$> maxView t

deleteMax :: NoTree k a -> NoTree k a
deleteMax = updateMax (\_ -> Nothing)

adjustMax :: (a -> a) -> NoTree k a -> NoTree k a
adjustMax f = adjustMaxWithKey (\_ -> f)

adjustMaxWithKey :: (k -> a -> a) -> NoTree k a -> NoTree k a
adjustMaxWithKey f = updateMaxWithKey (\k a -> Just $ f k a)

updateMax :: (a -> Maybe a) -> NoTree k a -> NoTree k a
updateMax f = updateMaxWithKey (\_ -> f)

updateMaxWithKey :: (k -> a -> Maybe a) -> NoTree k a -> NoTree k a
updateMaxWithKey f (NoTree as) =
  NoTree $
    case as of
      bs :|> (k, a) ->
        case f k a of
          Just b  -> bs :|> (k, b)
          Nothing -> bs

      Empty         -> Seq.empty

maxView :: NoTree k a -> Maybe (NoTree k a, k, a)
maxView (NoTree as) =
  case as of
    bs :|> (k, a) -> Just (NoTree bs, k, a)
    Empty         -> Nothing
