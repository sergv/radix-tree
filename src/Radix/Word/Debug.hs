module Radix.Word.Debug
  ( S (..)
  , validBelow
  ) where

import           Radix.Word.Foundation

import           Data.Bits



-- | Branch side.
data S = L -- ^ Left. Masked bit of the prefix above this node must be @0@.
       | R -- ^ Right. Masked bit of the prefix above this node must be @1@.
         deriving Show

-- | Check whether the key below aligns with the side the branch is on.
validBelow :: Prefix -> S -> Key -> Bool
validBelow p1 s p2 =
  let q = p2 .&. (p1 .&. negate p1)
  in not (beyond p1 p2) && case s of
                             L -> q == 0
                             R -> q /= 0
