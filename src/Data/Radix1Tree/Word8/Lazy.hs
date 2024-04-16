{-|
    @'LazyRadix1Tree' a@ is a spine-lazy radix tree that uses byte-aligned
    non-empty byte sequences as keys.

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

    Functions that produce and consume 'Feed1's are treated specially within the library,
    as when combined they can be reduced in a manner similar to the
    [destroy/unfoldr elimination rule](https://wiki.haskell.org/Correctness_of_short_cut_fusion#destroy.2Funfoldr).

    The elimination in this library is achieved by inlining both types of functions
    heavily. To avoid unnecessary code duplication during compilation consider creating
    helper functions that apply these functions one to another, e.g.

    @updateBS f bs = 'update' f ('Data.Radix1Tree.Word8.Key.Unsafe.unsafeFeedByteString' bs)@

    N.B. To inline properly functions that consume 'Feed1's must mention all of the
         arguments except for the tree.

    == Implementation

    See the implementation section in "Data.RadixTree.Word8.Strict.Unsafe"
    for the explanation of the innerworkings.

    See the implementation section in "Data.Patricia.Word.Strict" for literary references.
 -}

module Data.Radix1Tree.Word8.Lazy
  ( LazyRadix1Tree
  , Radix1Tree

  , RadixTree (..)

    -- * Key
  , module Data.Radix1Tree.Word8.Key

    -- * Construct
  , empty
  , singleton

    -- ** Convert
  , toStrict

    -- * Single-key
    -- ** Lookup
  , Data.Radix1Tree.Word8.Lazy.lookup
  , Data.Radix1Tree.Word8.Lazy.find
  , Data.Radix1Tree.Word8.Lazy.member
  , subtree

    -- *** Chunked
    --
    -- | Chunked lookup allows providing the key piece by piece while retaining
    --   the ability to check for early failure.
    --
    --   Note that while 'subtree' can be used to achieve the same result,
    --   it is more expensive allocation-wise, as it must ensure that
    --   the resulting tree is well-formed after each chunk application.
  , Cursor
  , cursor
  , move
  , stop
  , Location (..)
  , locate

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
  , Data.Radix1Tree.Word8.Lazy.null
  , size

    -- ** Extend
  , prefix

    -- ** Map
  , Data.Radix1Tree.Word8.Lazy.map
  , mapWithKey

    -- ** Fold
    -- | === Left-to-right
  , Data.Radix1Tree.Word8.Lazy.foldl
  , Data.Radix1Tree.Word8.Lazy.foldl'
  , foldlWithKey
  , foldlWithKey'

    -- | === Right-to-left
  , Data.Radix1Tree.Word8.Lazy.foldr
  , Data.Radix1Tree.Word8.Lazy.foldr'
  , foldrWithKey
  , foldrWithKey'

    -- | === Monoid
  , Data.Radix1Tree.Word8.Lazy.foldMap
  , foldMapWithKey

    -- ** Traverse
  , Data.Radix1Tree.Word8.Lazy.traverse
  , traverseWithKey

    -- ** Filter
    -- | === One side
  , Data.Radix1Tree.Word8.Lazy.filter
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
  , Data.Radix1Tree.Word8.Lazy.compare

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
    -- | See 'Data.Radix1Tree.Word8.Lazy.Unsafe.merge'.
  ) where

import           Data.Radix1Tree.Word8.Key
import           Data.RadixNTree.Word8.Common
import           Data.RadixNTree.Word8.Conversion
import           Data.RadixNTree.Word8.Lazy
import           Radix.Common



-- | \(\mathcal{O}(1)\).
--   Empty tree.
empty :: Radix1Tree a
empty = empty1

{-# INLINE singleton #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(x)\).
--   Tree with a single entry.
singleton :: Feed1 -> a -> Radix1Tree a
singleton = singleton1


-- | \(\mathcal{O}(n)\).
--   Create a strict 'Strict.Patricia' tree from a lazy one.
--
--   The resulting tree does not share its data representation with the original.
toStrict :: LazyRadix1Tree a -> StrictRadix1Tree a
toStrict = toStrict1



-- | \(\mathcal{O}(1)\).
--   Check if the tree is empty.
null :: Radix1Tree a -> Bool
null = null1

-- | \(\mathcal{O}(n)\).
--   Calculate the number of elements stored in the tree.
--   The returned number is guaranteed to be non-negative.
size :: Radix1Tree a -> Int
size = size1



-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n)\).
--   Apply a function to every value in the tree.
map :: (a -> b) -> Radix1Tree a -> Radix1Tree b
map = map1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n)\).
--   Apply a function to every value in the tree.
mapWithKey :: (Build1 -> a -> b) -> Radix1Tree a -> Radix1Tree b
mapWithKey = mapWithKey1



-- | \(\mathcal{O}(n_R)\).
--   Fold the tree left-to-right.
foldl :: (b -> a -> b) -> b -> Radix1Tree a -> b
foldl = Data.RadixNTree.Word8.Lazy.foldl1

-- | \(\mathcal{O}(n_R)\).
--   Fold the tree left-to-right.
foldlWithKey :: (b -> Build1 -> a -> b) -> b -> Radix1Tree a -> b
foldlWithKey = foldlWithKey1

-- | \(\mathcal{O}(n)\).
--   Fold the tree left-to-right with a strict accumulator.
foldl' :: (b -> a -> b) -> b -> Radix1Tree a -> b
foldl' = foldl1'

-- | \(\mathcal{O}(n)\).
--   Fold the tree left-to-right with a strict accumulator.
foldlWithKey' :: (b -> Build1 -> a -> b) -> b -> Radix1Tree a -> b
foldlWithKey' = foldlWithKey1'



-- | \(\mathcal{O}(n_L)\).
--   Fold the tree right-to-left.
foldr :: (a -> b -> b) -> b -> Radix1Tree a -> b
foldr = Data.RadixNTree.Word8.Lazy.foldr1

-- | \(\mathcal{O}(n_L)\).
--   Fold the tree right-to-left.
foldrWithKey :: (Build1 -> a -> b -> b) -> b -> Radix1Tree a -> b
foldrWithKey = foldrWithKey1

-- | \(\mathcal{O}(n)\).
--   Fold the tree right-to-left with a strict accumulator.
foldr' :: (a -> b -> b) -> b -> Radix1Tree a -> b
foldr' = foldr1'

-- | \(\mathcal{O}(n)\).
--   Fold the tree right-to-left with a strict accumulator.
foldrWithKey' :: (Build1 -> a -> b -> b) -> b -> Radix1Tree a -> b
foldrWithKey' = foldrWithKey1'



-- | \(\mathcal{O}(n_M)\).
--   Map each element in the tree to a monoid and combine the results.
foldMap :: Monoid m => (a -> m) -> Radix1Tree a -> m
foldMap = foldMap1

-- | \(\mathcal{O}(n_M)\).
--   Map each element in the tree to a monoid and combine the results.
foldMapWithKey :: Monoid m => (Build1 -> a -> m) -> Radix1Tree a -> m
foldMapWithKey = foldMapWithKey1



-- | \(\mathcal{O}(n)\).
--   Map each element in the tree to an action, evaluate these actions
--   left-to-right and collect the results.
traverse :: Applicative f => (a -> f b) -> Radix1Tree a -> f (Radix1Tree b)
traverse = traverse1

-- | \(\mathcal{O}(n)\).
--   Map each element in the tree to an action, evaluate these actions
--   left-to-right and collect the results.
traverseWithKey
  :: Applicative f => (Build1 -> a -> f b) -> Radix1Tree a -> f (Radix1Tree b)
traverseWithKey = traverseWithKey1



-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n)\).
--   Filter values that satisfy the value predicate.
filter :: (a -> Bool) -> Radix1Tree a -> Radix1Tree a
filter = filter1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n)\).
--   Filter values that satisfy the value predicate.
filterWithKey :: (Build1 -> a -> Bool) -> Radix1Tree a -> Radix1Tree a
filterWithKey = filterWithKey1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n)\).
--   Apply a function to every value in the tree and create one out of 'Just' values.
mapMaybe :: (a -> Maybe b) -> Radix1Tree a -> Radix1Tree b
mapMaybe = mapMaybe1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n)\).
--   Apply a function to every value in the tree and create one out of 'Just' values.
mapMaybeWithKey :: (Build1 -> a -> Maybe b) -> Radix1Tree a -> Radix1Tree b
mapMaybeWithKey = mapMaybeWithKey1


-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n)\).
--   Split the tree into two, such that values that satisfy the predicate
--   are on the left and values that do not are on the right.
partition :: (a -> Bool) -> Radix1Tree a -> (Radix1Tree a, Radix1Tree a)
partition = partition1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n)\).
--   Split the tree into two, such that values that satisfy the predicate
--   are on the left and values that do not are on the right.
partitionWithKey :: (Build1 -> a -> Bool) -> Radix1Tree a -> (Radix1Tree a, Radix1Tree a)
partitionWithKey = partitionWithKey1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n)\).
--   Apply a function to every value in the tree and create two trees,
--   one out of 'Left' results and one out of 'Right' ones.
mapEither :: (a -> Either b c) -> Radix1Tree a -> (Radix1Tree b, Radix1Tree c)
mapEither = mapEither1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n)\).
--   Apply a function to every value in the tree and create two trees,
--   one out of 'Left' results and one out of 'Right' ones.
mapEitherWithKey :: (Build1 -> a -> Either b c) -> Radix1Tree a -> (Radix1Tree b, Radix1Tree c)
mapEitherWithKey = mapEitherWithKey1



{-# INLINE lookup #-}
-- | \(\mathcal{O}(\min(x,k))\).
--   Look up the value at a key in the tree.
lookup :: Feed1 -> Radix1Tree a -> Maybe a
lookup = lookup1

{-# INLINE find #-}
-- | \(\mathcal{O}(\min(x,k))\).
--   Look up the value at a key in the tree, falling back to the given default value
--   if it does not exist.
find :: a -> Feed1 -> Radix1Tree a -> a
find = find1

{-# INLINE member #-}
-- | \(\mathcal{O}(\min(x,k))\).
--   Check whether the value exists at a key in the tree.
member :: Feed1 -> Radix1Tree a -> Bool
member = member1

{-# INLINE subtree #-}
-- | \(\mathcal{O}(\min(x,k))\).
--   Look up the part of the tree below the given prefix.
subtree :: Feed1 -> Radix1Tree a -> RadixTree a
subtree = subtree1

{-# INLINE prefix #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(x)\).
--   Prefix the root of the tree with the given key.
prefix :: Feed1 -> RadixTree a -> Radix1Tree a
prefix = prefix1


-- | \(\mathcal{O}(1)\).
--   Make a cursor that points to the root of the tree.
cursor :: Radix1Tree a -> Cursor a
cursor = cursor1

{-# INLINE move #-}
-- | \(\mathcal{O}(\min(x,k))\).
--   Move the cursor down by the extent of the given key.
move :: Feed1 -> Cursor a -> Cursor a
move = move1



{-# INLINE insert #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k))\).
--   Insert a new value in the tree at the given key.
--   If a value already exists at that key, it is replaced.
insert :: Feed1 -> a -> Radix1Tree a -> Radix1Tree a
insert = insert1

{-# INLINE insertWith #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k))\).
--   Insert a new value in the tree at the given key.
--   If a value already exists at that key, the function is used instead.
insertWith :: (a -> a) -> Feed1 -> a -> Radix1Tree a -> Radix1Tree a
insertWith = insertWith1


{-# INLINE adjust #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k))\).
--   Apply a function to a value in the tree at the given key.
adjust :: (a -> a) -> Feed1 -> Radix1Tree a -> Radix1Tree a
adjust = adjust1


{-# INLINE delete #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k))\).
--   Delete a value in the tree at the given key.
delete :: Feed1 -> Radix1Tree a -> Radix1Tree a
delete = delete1

{-# INLINE prune #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k))\).
--   Delete values in the tree below the given key.
prune :: Openness -> Feed1 -> Radix1Tree a -> Radix1Tree a
prune = prune1


{-# INLINE update #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k))\).
--   Update or delete a value in the tree at the given key.
update :: (a -> Maybe a) -> Feed1 -> Radix1Tree a -> Radix1Tree a
update = update1


{-# INLINE alter #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k))\).
--   Insert, update or delete a value in the tree at the given key.
alter :: (Maybe a -> Maybe a) -> Feed1 -> Radix1Tree a -> Radix1Tree a
alter = alter1


{-# INLINE shape #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k))\).
--   Update the part of the tree at the given prefix.
shape :: (RadixTree a -> RadixTree a) -> Feed1 -> Radix1Tree a -> Radix1Tree a
shape = shape1


{-# INLINE splitLookup #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k))\).
--   Split the tree into two, such that
--   values with keys smaller than the given one are on the left,
--   values with keys greater than the given one are on the right,
--   and the value at the given key is returned separately.
splitLookup :: Feed1 -> Radix1Tree a -> (Radix1Tree a, Maybe a, Radix1Tree a)
splitLookup = splitLookup1



{-# INLINE lookupL #-}
-- | \(\mathcal{O}(\min(x,k))\).
--   Look up a value at a largest key smaller than (or equal to) the given key.
lookupL :: Openness -> Feed1 -> Radix1Tree a -> Maybe (Lookup1 a)
lookupL = lookupL1


{-# INLINE lookupR #-}
-- | \(\mathcal{O}(\min(x,k))\).
--   Look up a value at a smallest key greater than (or equal to) the given key.
lookupR :: Openness -> Feed1 -> Radix1Tree a -> Maybe (Lookup1 a)
lookupR = lookupR1



{-# INLINE adjustL #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k) + n_L)\).
--   Apply a function to every value for which the key is smaller than
--   (or equal to) the given one.
adjustL :: (a -> a) -> Openness -> Feed1 -> Radix1Tree a -> Radix1Tree a
adjustL = adjustL1

{-# INLINE adjustLWithKey #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k) + n_L)\).
--   Apply a function to every value for which the key is smaller than
--   (or equal to) the given one.
adjustLWithKey :: (Build1 -> a -> a) -> Openness -> Feed1 -> Radix1Tree a -> Radix1Tree a
adjustLWithKey = adjustLWithKey1



{-# INLINE adjustR #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k) + n_R)\).
--   Apply a function to every value for which the key is greater than
--   (or equal to) the given one.
adjustR :: (a -> a) -> Openness -> Feed1 -> Radix1Tree a -> Radix1Tree a
adjustR = adjustR1

{-# INLINE adjustRWithKey #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k) + n_R)\).
--   Apply a function to every value for which the key is greater than
--   (or equal to) the given one.
adjustRWithKey :: (Build1 -> a -> a) -> Openness -> Feed1 -> Radix1Tree a -> Radix1Tree a
adjustRWithKey = adjustRWithKey1



{-# INLINE updateL #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k) + n_L)\).
--   Update every value for which the key is smaller than (or equal to) the given one.
updateL :: (a -> Maybe a) -> Openness -> Feed1 -> Radix1Tree a -> Radix1Tree a
updateL = updateL1

{-# INLINE updateLWithKey #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k) + n_L)\).
--   Update every value for which the key is smaller than (or equal to) the given one.
updateLWithKey :: (Build1 -> a -> Maybe a) -> Openness -> Feed1 -> Radix1Tree a -> Radix1Tree a
updateLWithKey = updateLWithKey1

{-# INLINE updateR #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k) + n_R)\).
--   Update every value for which the key is greater than (or equal to) the given one.
updateR :: (a -> Maybe a) -> Openness -> Feed1 -> Radix1Tree a -> Radix1Tree a
updateR = updateR1

{-# INLINE updateRWithKey #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k) + n_R)\).
--   Update every value for which the key is greater than (or equal to) the given one.
updateRWithKey :: (Build1 -> a -> Maybe a) -> Openness -> Feed1 -> Radix1Tree a -> Radix1Tree a
updateRWithKey = updateRWithKey1



{-# INLINE takeL #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k))\).
--   Take values for which keys are smaller than (or equal to) the given one.
takeL :: Openness -> Feed1 -> Radix1Tree a -> Radix1Tree a
takeL = takeL1

{-# INLINE takeR #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k))\).
--   Take values for which keys are greater than (or equal to) the given one.
takeR :: Openness -> Feed1 -> Radix1Tree a -> Radix1Tree a
takeR = takeR1



{-# INLINE splitL #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(\min(x,k))\).
--   Split the tree into two, such that
--   values with keys smaller than (or equal to) the given one are on the left,
--   and the rest are on the right.
splitL :: Openness -> Feed1 -> Radix1Tree a -> (Radix1Tree a, Radix1Tree a)
splitL = splitL1



-- | \(\mathcal{O}(k)\).
--   Look up a value at the leftmost key in the tree.
lookupMin :: Radix1Tree a -> Maybe a
lookupMin = lookupMin1

-- | \(\mathcal{O}(k)\).
--   Look up a value at the leftmost key in the tree.
lookupMinWithKey :: Radix1Tree a -> Maybe (Lookup1 a)
lookupMinWithKey = lookupMinWithKey1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(k)\).
--   Delete a value at the leftmost key in the tree.
deleteMin :: Radix1Tree a -> Radix1Tree a
deleteMin = deleteMin1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(k)\).
--   Update a value at the leftmost key in the tree.
adjustMin :: (a -> a) -> Radix1Tree a -> Radix1Tree a
adjustMin = adjustMin1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(k)\).
--   Update a value at the leftmost key in the tree.
adjustMinWithKey :: (Build1 -> a -> a) -> Radix1Tree a -> Radix1Tree a
adjustMinWithKey = adjustMinWithKey1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(k)\).
--   Update or delete a value at the leftmost key in the tree.
updateMin :: (a -> Maybe a) -> Radix1Tree a -> Radix1Tree a
updateMin = updateMin1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(k)\).
--   Update or delete a value at the leftmost key in the tree.
updateMinWithKey :: (Build1 -> a -> Maybe a) -> Radix1Tree a -> Radix1Tree a
updateMinWithKey = updateMinWithKey1

-- | \(\mathcal{O}(\min(x,k))\).
--   Look up the leftmost value and return it alongside the tree without it.
minView :: Radix1Tree a -> Maybe (ViewL1 a)
minView = minView1



-- | \(\mathcal{O}(k)\).
--   Look up a value at the rightmost key in the tree.
lookupMax :: Radix1Tree a -> Maybe a
lookupMax = lookupMax1

-- | \(\mathcal{O}(k)\).
--   Look up a value at the rightmost key in the tree.
lookupMaxWithKey :: Radix1Tree a -> Maybe (Lookup1 a)
lookupMaxWithKey = lookupMaxWithKey1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(k)\).
--   Delete a value at the rightmost key in the tree.
deleteMax :: Radix1Tree a -> Radix1Tree a
deleteMax = deleteMax1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(k)\).
--   Update a value at the rightmost key in the tree.
adjustMax :: (a -> a) -> Radix1Tree a -> Radix1Tree a
adjustMax = adjustMax1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(k)\).
--   Update a value at the rightmost key in the tree.
adjustMaxWithKey :: (Build1 -> a -> a) -> Radix1Tree a -> Radix1Tree a
adjustMaxWithKey = adjustMaxWithKey1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(k)\).
--   Update or delete a value at the rightmost key in the tree.
updateMax :: (a -> Maybe a) -> Radix1Tree a -> Radix1Tree a
updateMax = updateMax1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(k)\).
--   Update or delete a value at the rightmost key in the tree.
updateMaxWithKey :: (Build1 -> a -> Maybe a) -> Radix1Tree a -> Radix1Tree a
updateMaxWithKey = updateMaxWithKey1

-- | \(\mathcal{O}(\min(x,k))\).
--   Look up the rightmost value and return it alongside the tree without it.
maxView :: Radix1Tree a -> Maybe (ViewR1 a)
maxView = maxView1



-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n_A k_A + n_B k_B)\).
--   Unbiased union of two trees.
union :: Radix1Tree a -> Radix1Tree a -> Radix1Tree a
union = union1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n_A k_A + n_B k_B)\).
--   Left-biased union of two trees.
unionL :: Radix1Tree a -> Radix1Tree a -> Radix1Tree a
unionL = unionL1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n_A k_A + n_B k_B)\).
--   Union of two trees with a combining function.
unionWith :: (a -> a -> a) -> Radix1Tree a -> Radix1Tree a -> Radix1Tree a
unionWith = unionWith1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n_A k_A + n_B k_B)\).
--   Union of two trees with a combining function.
unionWithKey :: (Build1 -> a -> a -> a) -> Radix1Tree a -> Radix1Tree a -> Radix1Tree a
unionWithKey = unionWithKey1



-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n_A k_A + n_B k_B)\).
--   Difference of two trees.
difference :: Radix1Tree a -> Radix1Tree b -> Radix1Tree a
difference = difference1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n_A k_A + n_B k_B)\).
--   Difference of two trees with a combining function.
differenceWith :: (a -> b -> Maybe a) -> Radix1Tree a -> Radix1Tree b -> Radix1Tree a
differenceWith = differenceWith1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n_A k_A + n_B k_B)\).
--   Difference of two trees with a combining function.
differenceWithKey
  :: (Build1 -> a -> b -> Maybe a) -> Radix1Tree a -> Radix1Tree b -> Radix1Tree a
differenceWithKey = differenceWithKey1



-- | \(\mathcal{O}(n_A k_A + n_B k_B)\).
--   Compare two trees with respect to set inclusion,
--   using the given equality function for intersecting keys.
--   If any intersecting keys hold unequal values, the trees are 'Incomparable'.
compare :: (a -> b -> Bool) -> Radix1Tree a -> Radix1Tree b -> PartialOrdering
compare = compare1

-- | \(\mathcal{O}(n_A k_A + n_B k_B)\).
--   Determine whether two trees' key sets are disjoint.
disjoint :: Radix1Tree a -> Radix1Tree b -> Bool
disjoint = disjoint1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n_A k_A + n_B k_B)\).
--   Unbiased intersection of two trees.
intersection :: Radix1Tree a -> Radix1Tree a -> Radix1Tree a
intersection = intersection1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n_A k_A + n_B k_B)\).
--   Left-biased intersection of two trees.
intersectionL :: Radix1Tree a -> Radix1Tree b -> Radix1Tree a
intersectionL = intersectionL1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n_A k_A + n_B k_B)\).
--   Intersection of two trees with a combining function.
intersectionWith :: (a -> b -> c) -> Radix1Tree a -> Radix1Tree b -> Radix1Tree c
intersectionWith = intersectionWith1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n_A k_A + n_B k_B)\).
--   Intersection of two trees with a combining function.
intersectionWithKey :: (Build1 -> a -> b -> c) -> Radix1Tree a -> Radix1Tree b -> Radix1Tree c
intersectionWithKey = intersectionWithKey1
