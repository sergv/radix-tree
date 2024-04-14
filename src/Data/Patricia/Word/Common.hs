module Data.Patricia.Word.Common
  ( Lookup (..)
  ) where



-- | Key together with the value.
data Lookup a = Lookup {-# UNPACK #-} !Word a
                deriving Show
