{-| Compressed references for spine-strict radix trees.

    Pointers have a much smaller memory footprint and
    allow faster lookup in trees that are known to never change shape.
 -}

module Data.RadixTree.Word8.Strict.Pointer
  ( Pointer
  , pointer
  , follow
  ) where

import           Data.RadixNTree.Word8.Key (Feed)
import           Data.RadixNTree.Word8.Strict (RadixTree)
import           Data.RadixNTree.Word8.Strict.Pointer



{-# INLINE pointer #-}
-- | \(\mathcal{O}(\min(x,k))\).
--   Create a pointer that mirrors an existing key.
--
--   The pointer is only guaranteed to behave correctly for any tree that holds the
--   same set of keys as the provided one.
--
--   @since 1.1
pointer :: Feed -> RadixTree a -> Maybe Pointer
pointer = pointer0



-- | \(\mathcal{O}(\log n)\).
--   Look up the value at a pointer in the tree, falling back to the given default value
--   if it does not exist.
--
--   @since 1.1
follow :: a -> Pointer -> RadixTree a -> a
follow = follow0
