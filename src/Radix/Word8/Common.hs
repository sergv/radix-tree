module Radix.Word8.Common
  ( Location (..)
  ) where



-- | Whether the cursor point to a point within the tree.
data Location = Inside
              | Outside
                deriving Show
