module Data.RadixNTree.Word8.Common
  ( Lookup (..)
  , Lookup1 (..)

  , Openness (..)
  ) where

import           Data.RadixNTree.Word8.Key



-- | Key together with the value.
data Lookup a = Lookup !Build a
                deriving Show

-- | Key together with the value.
data Lookup1 a = Lookup1 !Build1 a
                 deriving Show



-- | Whether the endpoint itself is included in the interval.
data Openness = Open   -- ^ Excluding the point.
              | Closed -- ^ Including the point.
                deriving Show
