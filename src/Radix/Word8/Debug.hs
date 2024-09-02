module Radix.Word8.Debug
  ( S (..)

  , validPrefix
  , validKey
  ) where

import           Radix.Word8.Foundation

import           Data.Bits



-- | Branch side.
data S = L -- ^ Left. Masked bit of the prefix above this node must be @0@.
       | R -- ^ Right. Masked bit of the prefix above this node must be @1@.
         deriving Show



-- | Check whether the prefix below aligns with the side the branch is on.
validPrefix :: Prefix -> S -> Prefix -> Bool
validPrefix p s o =
  let low = p .&. negate p
  in even p && case s of
                 L -> o < p && p - o < low
                 R -> p < o && o - p < low



-- | Check whether the key below aligns with the side the branch is on.
validKey :: Prefix -> S -> Key -> Bool
validKey p s k =
  let low = p .&. negate p
  in case s of
       L -> k <  p && p - k <= low
       R -> p <= k && k - p <  low
