{-# OPTIONS_HADDOCK not-home #-}

{-|
    Data structure internals, helper operations and unsafe functions.
 -}

module Data.RadixTree.Word8.Lazy.Unsafe
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
import           Data.RadixNTree.Word8.Lazy
import           Radix.Exception
import           Radix.Word8.Foundation



-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n_A k_A + n_B k_B)\).
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
