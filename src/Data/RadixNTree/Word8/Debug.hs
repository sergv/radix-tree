module Data.RadixNTree.Word8.Debug
  ( Validity (..)
  , Reason (..)
  ) where

import           Data.RadixNTree.Word8.Key
import           Radix.Word8.Foundation



-- | Whether the tree is well-formed.
data Validity = Valid
              | Invalid Build Reason
                deriving Show

-- | Reason for why the tree is considered malformed.
data Reason = -- | Prefix is @0@.
              ZeroPrefix
              -- | Prefix below diverges from the prefix above.
            | PrefixBelow Prefix Prefix
              -- | Key diverges the prefix above.
            | KeyBelow Prefix Key
              -- | One of the branches is empty.
            | MalformedBin Prefix
              -- | Empty 'Data.Array.Byte.ByteArray'.
            | EmptyByteArray
              -- | @Tip@ stores no value and is not followed by a @Bin@.
            | UncompressedTip
              deriving Show
