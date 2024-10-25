{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_HADDOCK not-home #-}

{-|
    Data structure internals, helper operations and unsafe functions.
 -}

module Data.Radix1Tree.Word8.Lazy.Unsafe
  ( Radix1Tree (..)

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

    -- * Edges
    -- ** Lookup
  , Lookup1 (..)

    -- | === Min
  , unsafeLookupMin
  , unsafeLookupMinWithKey

    -- | === Max
  , unsafeLookupMax
  , unsafeLookupMaxWithKey

    -- ** Map
    -- | === Min
  , unsafeAdjustMin
  , unsafeAdjustMinWithKey

    -- | === Max
  , unsafeAdjustMax
  , unsafeAdjustMaxWithKey

    -- ** Delete
  , unsafeDeleteMin
  , unsafeDeleteMax

    -- ** Update
    -- | === Min
  , unsafeUpdateMin
  , unsafeUpdateMinWithKey

    -- | === Max
  , unsafeUpdateMax
  , unsafeUpdateMaxWithKey

    -- ** View
    -- | === Min
  , ViewL1 (..)
  , unsafeMinView

    -- | === Max
  , ViewR1 (..)
  , unsafeMaxView

    -- * Full-tree
    -- ** Merge
  , merge
  ) where

import           Data.RadixNTree.Word8.Key
import           Data.RadixNTree.Word8.Common
import           Data.RadixNTree.Word8.Lazy
import           Radix.Exception
import           Radix.Word8.Foundation



-- | \(\mathcal{O}(k)\).
--   Look up a value at the leftmost key in the tree.
--
--   Throws t'MalformedTree' if the tree is empty.
unsafeLookupMin :: Radix1Tree a -> (# a #)
unsafeLookupMin = unsafeLookupMin1

-- | \(\mathcal{O}(k)\).
--   Look up a value at the leftmost key in the tree.
--
--   Throws t'MalformedTree' if the tree is empty.
unsafeLookupMinWithKey :: Radix1Tree a -> Lookup1 a
unsafeLookupMinWithKey = unsafeLookupMinWithKey1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(k)\).
--   Delete a value at the leftmost key in the tree.
--
--   Throws t'MalformedTree' if the tree is empty.
unsafeDeleteMin :: Radix1Tree a -> Radix1Tree a
unsafeDeleteMin = unsafeDeleteMin1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(k)\).
--   Update a value at the leftmost key in the tree.
--
--   Throws t'MalformedTree' if the tree is empty.
unsafeAdjustMin :: (a -> a) -> Radix1Tree a -> Radix1Tree a
unsafeAdjustMin = unsafeAdjustMin1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(k)\).
--   Update a value at the leftmost key in the tree.
--
--   Throws t'MalformedTree' if the tree is empty.
unsafeAdjustMinWithKey :: (Build1 -> a -> a) -> Radix1Tree a -> Radix1Tree a
unsafeAdjustMinWithKey = unsafeAdjustMinWithKey1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(k)\).
--   Update or delete a value at the leftmost key in the tree.
--
--   Throws t'MalformedTree' if the tree is empty.
unsafeUpdateMin :: (a -> Maybe a) -> Radix1Tree a -> Radix1Tree a
unsafeUpdateMin = unsafeUpdateMin1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(k)\).
--   Update or delete a value at the leftmost key in the tree.
--
--   Throws t'MalformedTree' if the tree is empty.
unsafeUpdateMinWithKey :: (Build1 -> a -> Maybe a) -> Radix1Tree a -> Radix1Tree a
unsafeUpdateMinWithKey = unsafeUpdateMinWithKey1

-- | \(\mathcal{O}(\min(x,k))\).
--   Look up the leftmost value and return it alongside the tree without it.
--
--   Throws t'MalformedTree' if the tree is empty.
unsafeMinView :: Radix1Tree a -> ViewL1 a
unsafeMinView = unsafeMinView1



-- | \(\mathcal{O}(k)\).
--   Look up a value at the rightmost key in the tree.
--
--   Throws t'MalformedTree' if the tree is empty.
unsafeLookupMax :: Radix1Tree a -> (# a #)
unsafeLookupMax = unsafeLookupMax1

-- | \(\mathcal{O}(k)\).
--   Look up a value at the rightmost key in the tree.
--
--   Throws t'MalformedTree' if the tree is empty.
unsafeLookupMaxWithKey :: Radix1Tree a -> Lookup1 a
unsafeLookupMaxWithKey = unsafeLookupMaxWithKey1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(k)\).
--   Delete a value at the rightmost key in the tree.
--
--   Throws t'MalformedTree' if the tree is empty.
unsafeDeleteMax :: Radix1Tree a -> Radix1Tree a
unsafeDeleteMax = unsafeDeleteMax1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(k)\).
--   Update a value at the rightmost key in the tree.
--
--   Throws t'MalformedTree' if the tree is empty.
unsafeAdjustMax :: (a -> a) -> Radix1Tree a -> Radix1Tree a
unsafeAdjustMax = unsafeAdjustMax1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(k)\).
--   Update a value at the rightmost key in the tree.
--
--   Throws t'MalformedTree' if the tree is empty.
unsafeAdjustMaxWithKey :: (Build1 -> a -> a) -> Radix1Tree a -> Radix1Tree a
unsafeAdjustMaxWithKey = unsafeAdjustMaxWithKey1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(k)\).
--   Update or delete a value at the rightmost key in the tree.
--
--   Throws t'MalformedTree' if the tree is empty.
unsafeUpdateMax :: (a -> Maybe a) -> Radix1Tree a -> Radix1Tree a
unsafeUpdateMax = unsafeUpdateMax1

-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(k)\).
--   Update or delete a value at the rightmost key in the tree.
--
--   Throws t'MalformedTree' if the tree is empty.
unsafeUpdateMaxWithKey :: (Build1 -> a -> Maybe a) -> Radix1Tree a -> Radix1Tree a
unsafeUpdateMaxWithKey = unsafeUpdateMaxWithKey1

-- | \(\mathcal{O}(\min(x,k))\).
--   Look up the rightmost value and return it alongside the tree without it.
--
--   Throws t'MalformedTree' if the tree is empty.
unsafeMaxView :: Radix1Tree a -> ViewR1 a
unsafeMaxView = unsafeMaxView1



-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n_A k_A + n_B k_B)\).
--   General merge of two trees.
--
--   Resulting 'Maybe's and 'Radix1Tree's in argument functions are evaluated to WHNF.
--
--   This functions inlines when all argument functions are provided.
{-# INLINE merge #-}
merge
  :: (Build1 -> a -> b -> Maybe c)           -- ^ Single value collision
  -> (Build1 -> a -> Maybe c)                -- ^ Single left value
  -> (Build -> Radix1Tree a -> Radix1Tree c) -- ^ Left subtree
  -> (Build1 -> b -> Maybe c)                -- ^ Single right value
  -> (Build -> Radix1Tree b -> Radix1Tree c) -- ^ Right subtree
  -> Radix1Tree a
  -> Radix1Tree b
  -> Radix1Tree c
merge = merge1
