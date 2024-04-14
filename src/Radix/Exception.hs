module Radix.Exception
  ( MalformedTree (..)
  ) where

import           Control.Exception



-- | Exception thrown by functions that need to return a value,
--   but instead find an invariant-breaking empty node.
data MalformedTree = MalformedTree
                       String -- ^ Module name
                       String -- ^ Function name

instance Show MalformedTree where
  showsPrec _ (MalformedTree loc fun) =
    showString "radix-tree#"
      . showString loc . showChar '.'
      . showString fun . showString ": Encountered Nil, tree is malformed"

instance Exception MalformedTree
