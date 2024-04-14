{-# LANGUAGE PatternSynonyms #-}

{-|
    @'Zebra'@ is a fully-strict one-dimensional space partitioning tree,
    using 'Data.Word.Word's as keys.

    == Laziness

    Evaluating the root of the tree (i.e. @(_ :: 'Zebra')@) to
    weak head normal form evaluates the entire tree to normal form.

    == Performance

    Each function's time complexity is provided in the documentation.

    \(n\) refers to the total number of space partitions in the tree.
    Parts of the tree are denoted using subscripts: \(n_L\) refers to the left side,
    \(n_R\) to the right side, and \(n_I\) to a range (interval).

    \(W\) is the size of 'Word' in bits, i.e. @'Data.Bits.finiteBitSize' (0 :: 'Word')@.

    == Implementation

    See the implementation section in "Data.Zebra.Word.Unsafe" for the explanation of
    the innerworkings.

    See the implementation section in "Data.Patricia.Word.Strict" for literary references.
 -}

module Data.Zebra.Word
  ( Zebra
  , Color (..)

    -- * Construct
  , pattern Mono

    -- * Single-key
    -- ** Lookup
  , Data.Zebra.Word.Internal.lookup

    -- * Directional
    -- ** Size
    -- | === Left
  , monoL
  , sizeL

    -- | === Right
  , monoR
  , sizeR

    -- ** Lookup
    -- | === Left
  , lookupL
  , findL

    -- | === Right
  , lookupR
  , findR

    -- ** Insert
    -- | === Left
  , fillL

    -- | === Right
  , fillR

    -- ** Fold
    -- | === Left-to-right

    -- | ===== Left
  , foldlL
  , foldlL'

    -- | ===== Right
  , foldlR
  , foldlR'

    -- | === Right-to-left

    -- | ===== Left
  , foldrL
  , foldrL'

    -- | ===== Right
  , foldrR
  , foldrR'

    -- * Range
  , Range (Range)

    -- ** Size
  , monoRange
  , sizeRange

    -- ** Insert
  , fillRange

    -- ** Fold
    -- | === Left-to-right
  , foldlRange
  , foldlRange'

    -- | === Right-to-left
  , foldrRange
  , foldrRange'

    -- * Full tree
    -- ** Size
  , size

    -- ** Fold
    -- | === Left-to-right
  , Data.Zebra.Word.Internal.foldl
  , Data.Zebra.Word.Internal.foldl'

    -- | === Right-to-right
  , Data.Zebra.Word.Internal.foldr
  , Data.Zebra.Word.Internal.foldr'

    -- ** Complement
  , complement

    -- ** Compare
  , PartialOrdering (..)
  , Data.Zebra.Word.Internal.compare

    -- ** Union
  , union

    -- ** Difference
  , difference
  , symmetricDifference

    -- ** Intersection
  , disjoint
  , intersection
  ) where

import           Data.Zebra.Word.Internal
import           Radix.Common
