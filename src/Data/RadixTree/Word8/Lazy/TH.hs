{-# LANGUAGE TemplateHaskellQuotes #-}

{-|
    Template Haskell helper functions.
 -}

module Data.RadixTree.Word8.Lazy.TH
  ( sequenceCode
  ) where

import           Data.RadixNTree.Word8.Lazy.TH

import           Language.Haskell.TH.Syntax



-- | \(\mathcal{O}(n)\).
--   Evaluate a tree of typed expressions.
sequenceCode :: Quote m => RadixTree (Code m a) -> Code m (RadixTree a)
sequenceCode = sequenceCode0
