{-# OPTIONS_HADDOCK not-home #-}

{-|
    Data structure internals, helper operations and unsafe functions.

    == Implementation

    The tree is structurally identical to the
    'Data.Patricia.Word.Strict.Unsafe.Patricia' tree, holding 'Color's as values.

    A key \(k\) in the tree denotes a right-open interval
    \([k, k_R)\) within which every key has the same color as \(k\). \(k_R\) is the key
    immediately to the right of \(k\), or, if \(k\) is the rightmost key, \(+\infty\).

    Two adjacent intervals __must not__ have the same color. This both removes
    redundancies and allows to make assumptions about the color of the key
    immediately to the left.

    The following is a visual example of a possible 4-bit tree under these rules:

    ![4-bit tree](https://raw.githubusercontent.com/sergv/radix-tree/master/images/4bit.svg)
 -}

module Data.Zebra.Word.Unsafe
  ( Zebra (..)
  , Color (..)

    -- * Bit operations
  , Prefix
  , Key

    -- | === Compare
  , beyond
  , upper
  , lower

    -- | === Create
  , Mask
  , zeroBit
  , mask
  , branchingBit

    -- * Directional
    -- ** Size
  , unsafeSizeL
  , unsafeSizeR

    -- ** Insert
  , unsafeFillL

    -- * Range
  , Range (..)

    -- ** Size
  , unsafeMonoRange
  , unsafeSizeRange

    -- ** Insert
  , unsafeFillRange

    -- ** Fold
    -- | === Left-to-right
  , unsafeFoldlRange
  , unsafeFoldlRange'

    -- | === Right-to-left
  , unsafeFoldrRange
  , unsafeFoldrRange'

    -- * Full tree
    -- ** Size
  , unsafeSize
  ) where

import           Data.Zebra.Word.Internal
import           Radix.Word.Foundation
