{-# OPTIONS_HADDOCK not-home #-}

{-|
    Data structure internals, helper operations and unsafe functions.

    == Implementation

    The tree is an altered 'Data.Patricia.Word.Strict.Unsafe.Patricia' tree.

    Each 'Tip' in the radix tree represents a continuous non-empty chunk of the key,
    at the end of which there either exists a value or the rest of the key branches.
    The first byte of the chunk corresponds to a 'Key' in a
    'Data.Patricia.Word.Strict.Unsafe.Patricia' tree, hence the definitions of
    'Bin' and 'Nil' remain unchanged.

    The only state the resulting t'Radix1Tree' is unable to represent is the
    value at the root of the tree (for which the key is an empty byte sequence),
    as such that value is prepended with a special 2-tuple named t'RadixTree'.
 -}

module Data.RadixTree.Word8.Strict.Unsafe
  ( RadixTree (..)
  , Radix1Tree (..)

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

    -- * Full-tree
    -- ** Merge
  , merge
  ) where

import           Data.RadixNTree.Word8.Key
import           Data.RadixNTree.Word8.Strict
import           Radix.Exception
import           Radix.Word8.Foundation



-- | \(\mathcal{O}(n_A k_A + n_B k_B)\).
--   General merge of two trees.
--
--   Resulting 'Maybe's and 'Radix1Tree's in argument functions are evaluated to WHNF.
--
--   This functions inlines when all argument functions are provided.
{-# INLINE merge #-}
merge
  :: (Build -> a -> b -> Maybe c)            -- ^ Single value collision
  -> (Build -> a -> Maybe c)                 -- ^ Single left value
  -> (Build -> Radix1Tree a -> Radix1Tree c) -- ^ Left subtree
  -> (Build -> b -> Maybe c)                 -- ^ Single right value
  -> (Build -> Radix1Tree b -> Radix1Tree c) -- ^ Right subtree
  -> RadixTree a
  -> RadixTree b
  -> RadixTree c
merge = merge0
