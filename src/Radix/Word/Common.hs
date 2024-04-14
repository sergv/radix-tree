{-# LANGUAGE PatternSynonyms #-}

module Radix.Word.Common
  ( Range (Range, ..)
  ) where

import           Radix.Word.Foundation



-- | A closed interval between two keys.
data Range = -- | Invariant: \(k_L \le k_R\).
             UnsafeRange
               {-# UNPACK #-} !Key -- ^ \(k_L\)
               {-# UNPACK #-} !Key -- ^ \(k_R\)

instance Show Range where
  showsPrec d (UnsafeRange kL kR) =
    showParen (d > 10) $
      showString "Range " . shows kL
           . showChar ' ' . shows kR

{-# COMPLETE Range #-}
-- | Reorders endpoints to fit mathematical notation:
--   \([12, 3]\) will be converted to \([3, 12]\).
--
--   Pattern matching guarantees \(k_1 \le k_2\).
pattern Range
  :: Word  -- ^ \(k_1\)
  -> Word  -- ^ \(k_2\)
  -> Range
pattern Range kL kR <- UnsafeRange kL kR
  where
    Range k1 k2
      | k1 <= k2  = UnsafeRange k1 k2
      | otherwise = UnsafeRange k2 k1
