module Data.Patricia.Word.Debug
  ( Validity (..)
  , Reason (..)
  ) where

import           Radix.Word.Foundation



-- | Whether the tree is well-formed.
data Validity = Valid
              | Invalid Reason
                deriving Show

-- | Reason for why the tree is considered malformed.
data Reason = -- | Prefix is @0@.
              ZeroPrefix
            | -- | Prefix below diverges from the prefix above.
              PrefixBelow Prefix Prefix
              -- | Key diverges the prefix above.
            | KeyBelow Prefix Key
              -- | One of the branches is empty.
            | MalformedBin Prefix
              deriving Show
