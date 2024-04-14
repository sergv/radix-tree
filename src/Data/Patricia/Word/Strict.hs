{-|
    @'StrictPatricia' a@ is a spine-strict big-endian PATRICIA tree, a compressed
    binary trie, using 'Word's as keys.

    == Laziness

    Evaluating the root of the tree (i.e. @(_ :: 'StrictPatricia' a)@) to
    weak head normal form evaluates the entire spine of the tree to normal form.

    Functions do not perform any additional evaluations unless
    their documentation directly specifies so.

    == Performance

    Each function's time complexity is provided in the documentation.

    \(n\) refers to the total number of entries in the tree.
    Parts of the tree are denoted using subscripts: \(n_L\) refers to the left side,
    \(n_R\) to the right side, \(n_I\) to a range (interval), and
    \(n_M\) to entries collected with the use of a 'Monoid'.

    \(W\) is the size of 'Word' in bits, i.e. @'Data.Bits.finiteBitSize' (0 :: 'Word')@.

    == Implementation

    Description of the PATRICIA tree and some of the algorithms implemented can be found
    within the following paper:

      * Chris Okasaki and Andy Gill, "/Fast Mergeable Integer Maps/",
        Workshop on ML, September 1998, pages 77-86,
        <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.37.5452>
 -}

module Data.Patricia.Word.Strict
  ( StrictPatricia
  , Patricia

    -- * Construct
  , empty
  , singleton

    -- ** Convert
  , toLazy

    -- * Single-key
    -- ** Lookup
  , Data.Patricia.Word.Strict.Internal.lookup
  , Data.Patricia.Word.Strict.Internal.find
  , member

    -- *** Dirty
    --
    -- | Dirty lookups omit intermediate checks and are thus faster for keys
    --   that are in the tree, at the cost of being slower for keys not in the tree.
  , dirtyLookup
  , dirtyFind
  , dirtyMember

    -- ** Insert
  , insert
  , insertWith
  , insertWith'

    -- ** Map
  , adjust
  , adjust'

    -- ** Delete
  , delete

    -- ** Update
  , update

  , alter

    -- ** Take
  , SplitLookup (..)
  , splitLookup

    -- * Directional
    -- ** Lookup
  , Lookup (..)
  , lookupL
  , lookupR

    -- ** Map
    -- | === Left
  , adjustL
  , adjustL'
  , adjustLWithKey
  , adjustLWithKey'

    -- | === Right
  , adjustR
  , adjustR'
  , adjustRWithKey
  , adjustRWithKey'

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
  , Split (..)

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
  , adjustRange'

  , adjustRangeWithKey
  , adjustRangeWithKey'

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
  , adjustMin'
  , adjustMinWithKey
  , adjustMinWithKey'

    -- | === Max
  , adjustMax
  , adjustMax'
  , adjustMaxWithKey
  , adjustMaxWithKey'

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
  , Data.Patricia.Word.Strict.Internal.null
  , size

    -- ** Map
  , Data.Patricia.Word.Strict.Internal.map
  , map'
  , mapWithKey
  , mapWithKey'

    -- ** Fold
    -- | === Left-to-right
  , Data.Patricia.Word.Strict.Internal.foldl
  , Data.Patricia.Word.Strict.Internal.foldl'
  , foldlWithKey
  , foldlWithKey'

    -- | === Right-to-left
  , Data.Patricia.Word.Strict.Internal.foldr
  , Data.Patricia.Word.Strict.Internal.foldr'
  , foldrWithKey
  , foldrWithKey'

    -- | === Monoid
  , Data.Patricia.Word.Strict.Internal.foldMap
  , foldMapWithKey

    -- ** Traverse
  , Data.Patricia.Word.Strict.Internal.traverse
  , traverseWithKey

    -- ** Filter
    -- | === One side
  , Data.Patricia.Word.Strict.Internal.filter
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
  , Data.Patricia.Word.Strict.Internal.compare

    -- ** Union
  , union
  , unionL
  , unionWith'
  , unionWithKey'

    -- ** Difference
  , difference
  , differenceWith
  , differenceWithKey

    -- ** Intersection
  , disjoint
  , intersection
  , intersectionL
  , intersectionWith'
  , intersectionWithKey'

    -- ** Merge
    -- | See 'Data.Patricia.Word.Strict.Unsafe.merge'.
  ) where

import           Data.Patricia.Word.Common
import           Data.Patricia.Word.Conversion
import           Data.Patricia.Word.Strict.Internal
import           Radix.Common
import           Radix.Word.Common



-- | \(\mathcal{O}(1)\).
--   Empty tree.
empty :: Patricia a
empty = Nil

-- | \(\mathcal{O}(1)\).
--   Tree with a single entry.
singleton :: Word -> a -> Patricia a
singleton = Tip
