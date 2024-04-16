{-|
    @'LazyPatricia' a@ is a spine-lazy big-endian PATRICIA tree, a compressed
    trie with a radix of 2, using 'Word's as keys.

    == Laziness

    Evaluating any particular entry in the tree to WHNF forces the evaluation
    of the part of the spine leading up to that entry to normal form.

    == Performance

    Each function's time complexity is provided in the documentation.

    Laziness-amortized functions specify two time complexities:
    time to construct the return value (denoted with a \(\texttt{+}\)) and time to
    fully apply the function to the tree.

    \(n\) refers to the number of evaluated entries in the resulting tree.
    Parts of the tree are denoted using subscripts: \(n_L\) refers to the left side,
    \(n_R\) to the right side, \(n_I\) to a range (interval), and
    \(n_M\) to entries collected with the use of a 'Monoid'.

    \(W\) is the size of 'Word' in bits, i.e. @'Data.Bits.finiteBitSize' (0 :: 'Word')@.

    == Implementation

    See the implementation section in "Data.Patricia.Word.Strict".
 -}

module Data.Patricia.Word.Lazy
  ( LazyPatricia
  , Patricia

    -- * Construct
  , empty
  , singleton

    -- ** Convert
  , toStrict

    -- * Single-key
    -- ** Lookup
  , Data.Patricia.Word.Lazy.Internal.lookup
  , Data.Patricia.Word.Lazy.Internal.find
  , member

    -- ** Insert
  , insert
  , insertWith

    -- ** Map
  , adjust

    -- ** Delete
  , delete

    -- ** Update
  , update

  , alter

    -- ** Take
  , splitLookup

    -- * Directional
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

    -- ** Delete
  , deleteL
  , deleteR

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
  , splitR

    -- * Range
  , Range (Range)

    -- ** Map
  , adjustRange
  , adjustRangeWithKey

    -- ** Delete
  , deleteRange

    -- ** Update
  , updateRange
  , updateRangeWithKey

    -- ** Take
  , takeRange

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
  , Data.Patricia.Word.Lazy.Internal.null
  , size

    -- ** Map
  , Data.Patricia.Word.Lazy.Internal.map
  , mapWithKey

    -- ** Fold
    -- | === Left-to-right
  , Data.Patricia.Word.Lazy.Internal.foldl
  , Data.Patricia.Word.Lazy.Internal.foldl'
  , foldlWithKey
  , foldlWithKey'

    -- | === Right-to-left
  , Data.Patricia.Word.Lazy.Internal.foldr
  , Data.Patricia.Word.Lazy.Internal.foldr'
  , foldrWithKey
  , foldrWithKey'

    -- | === Monoid
  , Data.Patricia.Word.Lazy.Internal.foldMap
  , foldMapWithKey

    -- ** Traverse
  , Data.Patricia.Word.Lazy.Internal.traverse
  , traverseWithKey

    -- ** Filter
    -- | === One side
  , Data.Patricia.Word.Lazy.Internal.filter
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
  , Data.Patricia.Word.Lazy.Internal.compare

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
    -- | See 'Data.Patricia.Word.Lazy.Unsafe.merge'.
  ) where

import           Data.Patricia.Word.Common
import           Data.Patricia.Word.Conversion
import           Data.Patricia.Word.Lazy.Internal
import           Radix.Common
import           Radix.Word.Common



-- | \(\mathcal{O}(1)\). Empty tree.
empty :: Patricia a
empty = Nil

-- | \(\mathcal{O}(1)\). Tree with a single entry.
singleton :: Word -> a -> Patricia a
singleton = Tip
