{-# OPTIONS_HADDOCK not-home #-}

{-|
    Data structure internals, helper operations and unsafe functions.
 -}

module Data.Patricia.Word.Strict.Unsafe
  ( Patricia (..)

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

    -- * Exceptions
  , MalformedTree (..)

    -- * Range #range#
  , Range (..)

    -- ** Map
  , unsafeAdjustRange
  , unsafeAdjustRange'

  , unsafeAdjustRangeWithKey
  , unsafeAdjustRangeWithKey'

    -- ** Delete
  , unsafeDeleteRange

    -- ** Update
  , unsafeUpdateRange
  , unsafeUpdateRangeWithKey

    -- ** Take
  , unsafeTakeRange

    -- * Edges
    -- ** Lookup
  , Lookup (..)

    -- | === Min
  , unsafeLookupMin
  , unsafeLookupMinWithKey

    -- | === Max
  , unsafeLookupMax
  , unsafeLookupMaxWithKey

    -- ** View
    -- | === Min
  , ViewL (..)
  , unsafeMinView

    -- | === Max
  , ViewR (..)
  , unsafeMaxView

    -- * Full-tree
    -- ** Merge
  , merge
  ) where

import           Data.Patricia.Word.Common
import           Data.Patricia.Word.Strict.Internal
import           Radix.Exception
import           Radix.Word.Common
import           Radix.Word.Foundation
