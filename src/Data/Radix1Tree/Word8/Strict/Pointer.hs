{-| Compressed references for spine-strict radix trees.

    Pointers have a much smaller memory footprint and
    allow faster lookup in trees that are known to never change shape.
 -}

module Data.Radix1Tree.Word8.Strict.Pointer
  ( Pointer
  , pointer
  , follow
  ) where

import           Data.RadixNTree.Word8.Key (Feed1)
import           Data.RadixNTree.Word8.Strict (Radix1Tree)
import           Data.RadixNTree.Word8.Strict.Pointer



{-# INLINE pointer #-}
-- | \(\mathcal{O}(\min(x,k))\).
--   Create a pointer that mirrors an existing key.
--
--   The pointer is only guaranteed to behave correctly for any tree that holds the
--   same set of keys as the provided one.
--
--   @since 1.1
pointer :: Feed1 -> Radix1Tree a -> Maybe Pointer
pointer = pointer1



-- | \(\mathcal{O}(\log n)\).
--   Look up the value at a pointer in the tree, falling back to the given default value
--   if it does not exist.
--
--   @since 1.1
follow :: a -> Pointer -> Radix1Tree a -> a
follow = follow1
