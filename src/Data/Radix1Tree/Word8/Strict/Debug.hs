{-|
    Safe functions for datatype introspection.
 -}

module Data.Radix1Tree.Word8.Strict.Debug
  ( -- * Show
    showsTree

    -- * Validate
  , Validity (..)
  , Reason (..)
  , validate
  ) where

import           Data.RadixNTree.Word8.Strict (Radix1Tree)
import           Data.RadixNTree.Word8.Strict.Debug



-- | \(\mathcal{O}(n)\).
--   Shows the internal structure of the tree.
showsTree :: (a -> ShowS) -> Radix1Tree a -> ShowS
showsTree = showsTree1



-- | \(\mathcal{O}(n)\).
--   Checks whether the tree is well-formed.
validate :: Radix1Tree a -> Validity
validate = validate1
