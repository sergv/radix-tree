{-|
    Safe functions for datatype introspection.
 -}

module Data.RadixTree.Word8.Lazy.Debug
  ( -- * Show
    showsTree

    -- * Validate
  , Validity (..)
  , Reason (..)
  , validate
  ) where

import           Data.RadixNTree.Word8.Lazy (RadixTree)
import           Data.RadixNTree.Word8.Lazy.Debug



-- | \(\mathcal{O}(n)\).
--   Shows the internal structure of the tree.
showsTree :: (a -> ShowS) -> RadixTree a -> ShowS
showsTree = showsTree0



-- | \(\mathcal{O}(n)\).
--   Checks whether the tree is well-formed.
validate :: RadixTree a -> Validity
validate = validate0
