module Radix.Word8.Foundation
  ( Key
  , Prefix
  , Mask

  , beyond
  , upper
  , lower

  , zeroBit
  , mask
  , branchingBit
  ) where

import           Data.Bits
import           Data.Word



-- | Key as stored in the data structure.
type Key = Word8

-- | Part of the 'Key' from the largest bit to the 'Mask' bit, plus the 'Mask' bit.
type Prefix = Word8

{-# INLINE beyond #-}
-- | \(\mathcal{O}(1)\).
--   Whether the key does not match the prefix.
beyond :: Prefix -> Key -> Bool
beyond p k = (k `xor` p) .&. (p `xor` negate p) /= 0

{-# INLINE upper #-}
-- | \(\mathcal{O}(1)\).
--   Largest key that can reside under this prefix.
upper :: Prefix -> Key
upper p = p .|. (p - 1)

{-# INLINE lower #-}
-- | \(\mathcal{O}(1)\).
--   Smallest key that can reside under this prefix.
lower :: Prefix -> Key
lower p = p .&. (p - 1)



-- | Masking bit.
type Mask = Word8

{-# INLINE zeroBit #-}
-- | Get the state of the masked bit from the 'Key'.
zeroBit :: Key -> Mask -> Bool
zeroBit k m = (k .&. m) == 0

{-# INLINE mask #-}
-- | Trim the 'Key' down to a 'Prefix'.
mask :: Key -> Mask -> Prefix
mask k m = k .&. (negate m `xor` m)

{-# INLINE branchingBit #-}
-- | Finds the bit the two 'Prefix'es disagree on.
branchingBit :: Prefix -> Prefix -> Mask
branchingBit p o = 1 `unsafeShiftL` (7 - countLeadingZeros (p `xor` o))
