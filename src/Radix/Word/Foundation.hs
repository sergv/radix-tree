module Radix.Word.Foundation
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



-- | Key as stored in the data structure.
type Key = Word

-- | Part of the 'Key' from the largest bit to the 'Mask' bit, plus the 'Mask' bit.
type Prefix = Word

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
type Mask = Word

{-# INLINE zeroBit #-}
-- | \(\mathcal{O}(1)\).
--   Get the state of the masked bit from the 'Key'.
zeroBit :: Key -> Mask -> Bool
zeroBit k m = (k .&. m) == 0

{-# INLINE mask #-}
-- | \(\mathcal{O}(1)\).
--   Trim the 'Key' down to the masking bit.
mask :: Key -> Mask -> Word
mask k m = k .&. (negate m `xor` m)

{-# INLINE branchingBit #-}
-- | \(\mathcal{O}(1)\).
--   Find the bit two 'Prefix'es disagree on.
--
--   Note that using this function on two equal integers yields @1 << (-1)@,
--   which results in undefined behavior.
branchingBit :: Prefix -> Prefix -> Mask
branchingBit p o =
  1 `unsafeShiftL` (finiteBitSize (0 :: Word) - 1 - countLeadingZeros (p `xor` o))
