{-|
    @'LazyRadixTree' a@ is a spine-lazy radix tree that uses byte-aligned
    byte sequences as keys.

    == Laziness

    Evaluating any particular entry in the tree to WHNF forces the evaluation
    of the part of the spine leading up to that entry to normal form.

    == Performance

    Each function's time complexity is provided in the documentation.

    Laziness-amortized functions specify two time complexities:
    time to construct the return value (denoted with a \(\texttt{+}\)) and time to
    fully apply the function to the tree.

    \(x\) is the length of the input key.

    \(k\) is the length of the longest key stored in the tree.

    \(n\) refers to the total number of entries in the tree.
    Parts of the tree are denoted using subscripts: \(n_L\) refers to the left side,
    \(n_R\) to the right side, and \(n_M\) to entries collected with the use of a 'Monoid'.

    == Inlining

    Functions that produce and consume 'Feed's are treated specially within the library,
    as when combined they can be reduced in a manner similar to the
    [destroy/unfoldr elimination rule](https://wiki.haskell.org/Correctness_of_short_cut_fusion#destroy.2Funfoldr).

    The elimination in this library is achieved by inlining both types of functions
    heavily. To avoid unnecessary code duplication during compilation consider creating
    helper functions that apply these functions one to another, e.g.

    @updateBS f bs = 'update' f ('Data.RadixTree.Word8.Key.feedByteString' bs)@

    N.B. To inline properly functions that consume 'Feed's must mention all of the
         arguments except for the tree.

    == Implementation

    See the implementation section in "Data.RadixTree.Word8.Strict.Unsafe"
    for the explanation of the innerworkings.

    See the implementation section in "Data.Patricia.Word.Strict" for literary references.
 -}

module Data.RadixTree.Word8.Lazy
  ( LazyRadixTree
  , RadixTree (..)

    -- * Key
  , module Data.RadixTree.Word8.Key

    -- * Construct
  , empty
  , singleton

    -- ** Convert
  , toStrict

    -- * Single-key
    -- ** Lookup
  , Data.RadixTree.Word8.Lazy.lookup
  , Data.RadixTree.Word8.Lazy.find
  , Data.RadixTree.Word8.Lazy.member
  , subtree

    -- ** Insert
  , insert
  , insertWith

    -- ** Map
  , adjust

    -- ** Delete
  , delete
  , prune

    -- ** Update
  , update
  , alter
  , shape

    -- ** Take
  , splitLookup

    -- * Directional
  , Openness (..)

    -- ** Lookup
  , Lookup (..)
  , lookupL
  , lookupR

    -- ** Map
    -- | === Left
  , adjustL
  , adjustLWithKey

    -- | === Right
  , adjustR
  , adjustRWithKey

    -- ** Update
    -- | === Left
  , updateL
  , updateLWithKey

    -- | === Right
  , updateR
  , updateRWithKey

    -- ** Take
    -- | === Left
  , takeL
  , splitL

    -- | === Right
  , takeR

    -- * Edges

    -- ** Lookup
    -- | === Min
  , lookupMin
  , lookupMinWithKey

    -- | === Max
  , lookupMax
  , lookupMaxWithKey

    -- ** Map
    -- | === Min
  , adjustMin
  , adjustMinWithKey

    -- | === Max
  , adjustMax
  , adjustMaxWithKey

    -- ** Delete
  , deleteMin
  , deleteMax

    -- ** Update
    -- | === Min
  , updateMin
  , updateMinWithKey

    -- | === Max
  , updateMax
  , updateMaxWithKey

    -- ** View
    -- | === Min
  , ViewL (..)
  , minView

    -- | === Max
  , ViewR (..)
  , maxView

    -- * Full tree
    -- ** Size
  , Data.RadixTree.Word8.Lazy.null
  , size

    -- ** Extend
  , prefix

    -- ** Map
  , Data.RadixTree.Word8.Lazy.map
  , mapWithKey

    -- ** Fold
    -- | === Left-to-right
  , Data.RadixTree.Word8.Lazy.foldl
  , Data.RadixTree.Word8.Lazy.foldl'
  , foldlWithKey
  , foldlWithKey'

    -- | === Right-to-left
  , Data.RadixTree.Word8.Lazy.foldr
  , Data.RadixTree.Word8.Lazy.foldr'
  , foldrWithKey
  , foldrWithKey'

    -- | === Monoid
  , Data.RadixTree.Word8.Lazy.foldMap
  , foldMapWithKey

    -- ** Traverse
  , Data.RadixTree.Word8.Lazy.traverse
  , traverseWithKey

    -- ** Filter
    -- | === One side
  , Data.RadixTree.Word8.Lazy.filter
  , filterWithKey

  , mapMaybe
  , mapMaybeWithKey

    -- | === Both sides
  , partition
  , partitionWithKey

  , mapEither
  , mapEitherWithKey

    -- ** Comparison
  , PartialOrdering (..)
  , Data.RadixTree.Word8.Lazy.compare

    -- ** Union
  , union
  , unionL
  , unionWith
  , unionWithKey

    -- ** Difference
  , difference
  , differenceWith
  , differenceWithKey

    -- ** Intersection
  , disjoint
  , intersection
  , intersectionL
  , intersectionWith
  , intersectionWithKey

    -- ** Merge
    -- | See 'Data.RadixTree.Word8.Lazy.Unsafe.merge'.
  ) where

import           Data.RadixTree.Word8.Key
import           Data.RadixNTree.Word8.Common
import           Data.RadixNTree.Word8.Conversion
import           Data.RadixNTree.Word8.Lazy
import           Radix.Common



-- | \(\mathcal{O}(1)\).
--   Empty tree.
empty :: RadixTree a
empty = empty0

{-# INLINE singleton #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(x)\).
--   Tree with a single entry.
singleton :: Feed -> a -> RadixTree a
singleton = singleton0


-- | \(\mathcal{O}(n)\).
--   Create a strict 'Strict.Patricia' tree from a lazy one.
--
--   The resulting tree does not share its data representation with the original.
toStrict :: LazyRadixTree a -> StrictRadixTree a
toStrict = toStrict0



-- | \(\mathcal{O}(1)\).
--   Check if the tree is empty.
null :: RadixTree a -> Bool
null = null0

-- | \(\mathcal{O}(n)\).
--   Calculate the number of elements stored in the tree.
--   The returned number is guaranteed to be non-negative.
size :: RadixTree a -> Int
size = size0



-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n)\).
--   Apply a function to every value in the tree.
map :: (a -> b) -> RadixTree a -> RadixTree b
map = map0

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n)\).
--   Apply a function to every value in the tree.
mapWithKey :: (Build -> a -> b) -> RadixTree a -> RadixTree b
mapWithKey = mapWithKey0



-- | \(\mathcal{O}(n_R)\).
--   Fold the tree left-to-right.
foldl :: (b -> a -> b) -> b -> RadixTree a -> b
foldl = Data.RadixNTree.Word8.Lazy.foldl0

-- | \(\mathcal{O}(n_R)\).
--   Fold the tree left-to-right.
foldlWithKey :: (b -> Build -> a -> b) -> b -> RadixTree a -> b
foldlWithKey = foldlWithKey0

-- | \(\mathcal{O}(n)\).
--   Fold the tree left-to-right with a strict accumulator.
foldl' :: (b -> a -> b) -> b -> RadixTree a -> b
foldl' = foldl0'

-- | \(\mathcal{O}(n)\).
--   Fold the tree left-to-right with a strict accumulator.
foldlWithKey' :: (b -> Build -> a -> b) -> b -> RadixTree a -> b
foldlWithKey' = foldlWithKey0'



-- | \(\mathcal{O}(n_L)\).
--   Fold the tree right-to-left.
foldr :: (a -> b -> b) -> b -> RadixTree a -> b
foldr = Data.RadixNTree.Word8.Lazy.foldr0

-- | \(\mathcal{O}(n_L)\).
--   Fold the tree right-to-left.
foldrWithKey :: (Build -> a -> b -> b) -> b -> RadixTree a -> b
foldrWithKey = foldrWithKey0

-- | \(\mathcal{O}(n)\).
--   Fold the tree right-to-left with a strict accumulator.
foldr' :: (a -> b -> b) -> b -> RadixTree a -> b
foldr' = foldr0'

-- | \(\mathcal{O}(n)\).
--   Fold the tree right-to-left with a strict accumulator.
foldrWithKey' :: (Build -> a -> b -> b) -> b -> RadixTree a -> b
foldrWithKey' = foldrWithKey0'



-- | \(\mathcal{O}(n_M)\).
--   Map each element in the tree to a monoid and combine the results.
foldMap :: Monoid m => (a -> m) -> RadixTree a -> m
foldMap = foldMap0

-- | \(\mathcal{O}(n_M)\).
--   Map each element in the tree to a monoid and combine the results.
foldMapWithKey :: Monoid m => (Build -> a -> m) -> RadixTree a -> m
foldMapWithKey = foldMapWithKey0



-- | \(\mathcal{O}(n)\).
--   Map each element in the tree to an action, evaluate these actions
--   left-to-right and collect the results.
traverse :: Applicative f => (a -> f b) -> RadixTree a -> f (RadixTree b)
traverse = traverse0

-- | \(\mathcal{O}(n)\).
--   Map each element in the tree to an action, evaluate these actions
--   left-to-right and collect the results.
traverseWithKey
  :: Applicative f => (Build -> a -> f b) -> RadixTree a -> f (RadixTree b)
traverseWithKey = traverseWithKey0



-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n)\).
--   Filter values that satisfy the value predicate.
filter :: (a -> Bool) -> RadixTree a -> RadixTree a
filter = filter0

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n)\).
--   Filter values that satisfy the value predicate.
filterWithKey :: (Build -> a -> Bool) -> RadixTree a -> RadixTree a
filterWithKey = filterWithKey0

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n)\).
--   Apply a function to every value in the tree and create one out of 'Just' values.
mapMaybe :: (a -> Maybe b) -> RadixTree a -> RadixTree b
mapMaybe = mapMaybe0

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n)\).
--   Apply a function to every value in the tree and create one out of 'Just' values.
mapMaybeWithKey :: (Build -> a -> Maybe b) -> RadixTree a -> RadixTree b
mapMaybeWithKey = mapMaybeWithKey0


-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n)\).
--   Split the tree into two, such that values that satisfy the predicate
--   are on the left and values that do not are on the right.
partition :: (a -> Bool) -> RadixTree a -> (RadixTree a, RadixTree a)
partition = partition0

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n)\).
--   Split the tree into two, such that values that satisfy the predicate
--   are on the left and values that do not are on the right.
partitionWithKey :: (Build -> a -> Bool) -> RadixTree a -> (RadixTree a, RadixTree a)
partitionWithKey = partitionWithKey0

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n)\).
--   Apply a function to every value in the tree and create two trees,
--   one out of 'Left' results and one out of 'Right' ones.
mapEither :: (a -> Either b c) -> RadixTree a -> (RadixTree b, RadixTree c)
mapEither = mapEither0

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n)\).
--   Apply a function to every value in the tree and create two trees,
--   one out of 'Left' results and one out of 'Right' ones.
mapEitherWithKey :: (Build -> a -> Either b c) -> RadixTree a -> (RadixTree b, RadixTree c)
mapEitherWithKey = mapEitherWithKey0



{-# INLINE lookup #-}
-- | \(\mathcal{O}(\min(x,k))\).
--   Look up the value at a key in the tree.
lookup :: Feed -> RadixTree a -> Maybe a
lookup = lookup0

{-# INLINE find #-}
-- | \(\mathcal{O}(\min(x,k))\).
--   Look up the value at a key in the tree, falling back to the given default value
--   if it does not exist.
find :: a -> Feed -> RadixTree a -> a
find = find0

{-# INLINE member #-}
-- | \(\mathcal{O}(\min(x,k))\).
--   Check whether the value exists at a key in the tree.
member :: Feed -> RadixTree a -> Bool
member = member0

{-# INLINE subtree #-}
-- | \(\mathcal{O}(\min(x,k))\).
--   Look up the part of the tree below the given prefix.
subtree :: Feed -> RadixTree a -> RadixTree a
subtree = subtree0

{-# INLINE prefix #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(x)\).
--   Prefix the root of the tree with the given key.
prefix :: Feed -> RadixTree a -> RadixTree a
prefix = prefix0



{-# INLINE insert #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k))\).
--   Insert a new value in the tree at the given key.
--   If a value already exists at that key, it is replaced.
insert :: Feed -> a -> RadixTree a -> RadixTree a
insert = insert0

{-# INLINE insertWith #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k))\).
--   Insert a new value in the tree at the given key.
--   If a value already exists at that key, the function is used instead.
insertWith :: (a -> a) -> Feed -> a -> RadixTree a -> RadixTree a
insertWith = insertWith0


{-# INLINE adjust #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k))\).
--   Apply a function to a value in the tree at the given key.
adjust :: (a -> a) -> Feed -> RadixTree a -> RadixTree a
adjust = adjust0


{-# INLINE delete #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k))\).
--   Delete a value in the tree at the given key.
delete :: Feed -> RadixTree a -> RadixTree a
delete = delete0

{-# INLINE prune #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k))\).
--   Delete values in the tree below the given key.
prune :: Openness -> Feed -> RadixTree a -> RadixTree a
prune = prune0


{-# INLINE update #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k))\).
--   Update or delete a value in the tree at the given key.
update :: (a -> Maybe a) -> Feed -> RadixTree a -> RadixTree a
update = update0


{-# INLINE alter #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k))\).
--   Insert, update or delete a value in the tree at the given key.
alter :: (Maybe a -> Maybe a) -> Feed -> RadixTree a -> RadixTree a
alter = alter0


{-# INLINE shape #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k))\).
--   Update the part of the tree at the given prefix.
shape :: (RadixTree a -> RadixTree a) -> Feed -> RadixTree a -> RadixTree a
shape = shape0


{-# INLINE splitLookup #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k))\).
--   Split the tree into two, such that
--   values with keys smaller than the given one are on the left,
--   values with keys greater than the given one are on the right,
--   and the value at the given key is returned separately.
splitLookup :: Feed -> RadixTree a -> (RadixTree a, Maybe a, RadixTree a)
splitLookup = splitLookup0



{-# INLINE lookupL #-}
-- | \(\mathcal{O}(\min(x,k))\).
--   Look up a value at a largest key smaller than (or equal to) the given key.
lookupL :: Openness -> Feed -> RadixTree a -> Maybe (Lookup a)
lookupL = lookupL0


{-# INLINE lookupR #-}
-- | \(\mathcal{O}(\min(x,k))\).
--   Look up a value at a smallest key greater than (or equal to) the given key.
lookupR :: Openness -> Feed -> RadixTree a -> Maybe (Lookup a)
lookupR = lookupR0



{-# INLINE adjustL #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k) + n_L)\).
--   Apply a function to every value for which the key is smaller than
--   (or equal to) the given one.
adjustL :: (a -> a) -> Openness -> Feed -> RadixTree a -> RadixTree a
adjustL = adjustL0

{-# INLINE adjustLWithKey #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k) + n_L)\).
--   Apply a function to every value for which the key is smaller than
--   (or equal to) the given one.
adjustLWithKey :: (Build -> a -> a) -> Openness -> Feed -> RadixTree a -> RadixTree a
adjustLWithKey = adjustLWithKey0



{-# INLINE adjustR #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k) + n_R)\).
--   Apply a function to every value for which the key is greater than
--   (or equal to) the given one.
adjustR :: (a -> a) -> Openness -> Feed -> RadixTree a -> RadixTree a
adjustR = adjustR0

{-# INLINE adjustRWithKey #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k) + n_R)\).
--   Apply a function to every value for which the key is greater than
--   (or equal to) the given one.
adjustRWithKey :: (Build -> a -> a) -> Openness -> Feed -> RadixTree a -> RadixTree a
adjustRWithKey = adjustRWithKey0



{-# INLINE updateL #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k) + n_L)\).
--   Update every value for which the key is smaller than (or equal to) the given one.
updateL :: (a -> Maybe a) -> Openness -> Feed -> RadixTree a -> RadixTree a
updateL = updateL0

{-# INLINE updateLWithKey #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k) + n_L)\).
--   Update every value for which the key is smaller than (or equal to) the given one.
updateLWithKey :: (Build -> a -> Maybe a) -> Openness -> Feed -> RadixTree a -> RadixTree a
updateLWithKey = updateLWithKey0

{-# INLINE updateR #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k) + n_R)\).
--   Update every value for which the key is greater than (or equal to) the given one.
updateR :: (a -> Maybe a) -> Openness -> Feed -> RadixTree a -> RadixTree a
updateR = updateR0

{-# INLINE updateRWithKey #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k) + n_R)\).
--   Update every value for which the key is greater than (or equal to) the given one.
updateRWithKey :: (Build -> a -> Maybe a) -> Openness -> Feed -> RadixTree a -> RadixTree a
updateRWithKey = updateRWithKey0



{-# INLINE takeL #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k))\).
--   Take values for which keys are smaller than (or equal to) the given one.
takeL :: Openness -> Feed -> RadixTree a -> RadixTree a
takeL = takeL0

{-# INLINE takeR #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k))\).
--   Take values for which keys are greater than (or equal to) the given one.
takeR :: Openness -> Feed -> RadixTree a -> RadixTree a
takeR = takeR0



{-# INLINE splitL #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k))\).
--   Split the tree into two, such that
--   values with keys smaller than (or equal to) the given one are on the left,
--   and the rest are on the right.
splitL :: Openness -> Feed -> RadixTree a -> (RadixTree a, RadixTree a)
splitL = splitL0



-- | \(\mathcal{O}(k)\).
--   Look up a value at the leftmost key in the tree.
lookupMin :: RadixTree a -> Maybe a
lookupMin = lookupMin0

-- | \(\mathcal{O}(k)\).
--   Look up a value at the leftmost key in the tree.
lookupMinWithKey :: RadixTree a -> Maybe (Lookup a)
lookupMinWithKey = lookupMinWithKey0

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(k)\).
--   Delete a value at the leftmost key in the tree.
deleteMin :: RadixTree a -> RadixTree a
deleteMin = deleteMin0

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(k)\).
--   Update a value at the leftmost key in the tree.
adjustMin :: (a -> a) -> RadixTree a -> RadixTree a
adjustMin = adjustMin0

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(k)\).
--   Update a value at the leftmost key in the tree.
adjustMinWithKey :: (Build -> a -> a) -> RadixTree a -> RadixTree a
adjustMinWithKey = adjustMinWithKey0

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(k)\).
--   Update or delete a value at the leftmost key in the tree.
updateMin :: (a -> Maybe a) -> RadixTree a -> RadixTree a
updateMin = updateMin0

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(k)\).
--   Update or delete a value at the leftmost key in the tree.
updateMinWithKey :: (Build -> a -> Maybe a) -> RadixTree a -> RadixTree a
updateMinWithKey = updateMinWithKey0

-- | \(\mathcal{O}(\min(x,k))\).
--   Look up the leftmost value and return it alongside the tree without it.
minView :: RadixTree a -> Maybe (ViewL a)
minView = minView0



-- | \(\mathcal{O}(k)\).
--   Look up a value at the rightmost key in the tree.
lookupMax :: RadixTree a -> Maybe a
lookupMax = lookupMax0

-- | \(\mathcal{O}(k)\).
--   Look up a value at the rightmost key in the tree.
lookupMaxWithKey :: RadixTree a -> Maybe (Lookup a)
lookupMaxWithKey = lookupMaxWithKey0

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(k)\).
--   Delete a value at the rightmost key in the tree.
deleteMax :: RadixTree a -> RadixTree a
deleteMax = deleteMax0

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(k)\).
--   Update a value at the rightmost key in the tree.
adjustMax :: (a -> a) -> RadixTree a -> RadixTree a
adjustMax = adjustMax0

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(k)\).
--   Update a value at the rightmost key in the tree.
adjustMaxWithKey :: (Build -> a -> a) -> RadixTree a -> RadixTree a
adjustMaxWithKey = adjustMaxWithKey0

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(k)\).
--   Update or delete a value at the rightmost key in the tree.
updateMax :: (a -> Maybe a) -> RadixTree a -> RadixTree a
updateMax = updateMax0

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(k)\).
--   Update or delete a value at the rightmost key in the tree.
updateMaxWithKey :: (Build -> a -> Maybe a) -> RadixTree a -> RadixTree a
updateMaxWithKey = updateMaxWithKey0

-- | \(\mathcal{O}(\min(x,k))\).
--   Look up the rightmost value and return it alongside the tree without it.
maxView :: RadixTree a -> Maybe (ViewR a)
maxView = maxView0



-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n_A k_A + n_B k_B)\).
--   Unbiased union of two trees.
union :: RadixTree a -> RadixTree a -> RadixTree a
union = union0

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n_A k_A + n_B k_B)\).
--   Left-biased union of two trees.
unionL :: RadixTree a -> RadixTree a -> RadixTree a
unionL = unionL0

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n_A k_A + n_B k_B)\).
--   Union of two trees with a combining function.
unionWith :: (a -> a -> a) -> RadixTree a -> RadixTree a -> RadixTree a
unionWith = unionWith0

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n_A k_A + n_B k_B)\).
--   Union of two trees with a combining function.
unionWithKey :: (Build -> a -> a -> a) -> RadixTree a -> RadixTree a -> RadixTree a
unionWithKey = unionWithKey0



-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n_A k_A + n_B k_B)\).
--   Difference of two trees.
difference :: RadixTree a -> RadixTree b -> RadixTree a
difference = difference0

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n_A k_A + n_B k_B)\).
--   Difference of two trees with a combining function.
differenceWith :: (a -> b -> Maybe a) -> RadixTree a -> RadixTree b -> RadixTree a
differenceWith = differenceWith0

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n_A k_A + n_B k_B)\).
--   Difference of two trees with a combining function.
differenceWithKey
  :: (Build -> a -> b -> Maybe a) -> RadixTree a -> RadixTree b -> RadixTree a
differenceWithKey = differenceWithKey0



-- | \(\mathcal{O}(n_A k_A + n_B k_B)\).
--   Compare two trees with respect to set inclusion,
--   using the given equality function for intersecting keys.
--   If any intersecting keys hold unequal values, the trees are 'Incomparable'.
compare :: (a -> b -> Bool) -> RadixTree a -> RadixTree b -> PartialOrdering
compare = compare0

-- | \(\mathcal{O}(n_A k_A + n_B k_B)\).
--   Determine whether two trees' key sets are disjoint.
disjoint :: RadixTree a -> RadixTree b -> Bool
disjoint = disjoint0

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n_A k_A + n_B k_B)\).
--   Unbiased intersection of two trees.
intersection :: RadixTree a -> RadixTree a -> RadixTree a
intersection = intersection0

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n_A k_A + n_B k_B)\).
--   Left-biased intersection of two trees.
intersectionL :: RadixTree a -> RadixTree b -> RadixTree a
intersectionL = intersectionL0

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n_A k_A + n_B k_B)\).
--   Intersection of two trees with a combining function.
intersectionWith :: (a -> b -> c) -> RadixTree a -> RadixTree b -> RadixTree c
intersectionWith = intersectionWith0

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n_A k_A + n_B k_B)\).
--   Intersection of two trees with a combining function.
intersectionWithKey :: (Build -> a -> b -> c) -> RadixTree a -> RadixTree b -> RadixTree c
intersectionWithKey = intersectionWithKey0
