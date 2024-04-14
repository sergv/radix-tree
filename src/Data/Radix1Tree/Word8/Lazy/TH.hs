{-# LANGUAGE TemplateHaskellQuotes #-}

{-|
    Template Haskell helper functions.
 -}

module Data.Radix1Tree.Word8.Lazy.TH
  ( sequenceCode
  ) where

import           Data.RadixNTree.Word8.Lazy.TH

import           Language.Haskell.TH.Syntax



-- | \(\mathcal{O}(n)\).
--   Evaluate a tree of typed expressions.
sequenceCode :: Quote m => Radix1Tree (Code m a) -> Code m (Radix1Tree a)
sequenceCode = sequenceCode1
