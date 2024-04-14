{-# LANGUAGE TemplateHaskellQuotes #-}

{-|
    Template Haskell helper functions.
 -}

module Data.RadixTree.Word8.Strict.TH
  ( sequenceCode
  ) where

import           Data.RadixNTree.Word8.Strict.TH

import           Language.Haskell.TH.Syntax



-- | \(\mathcal{O}(n)\).
--   Evaluate a tree of typed expressions.
sequenceCode :: Quote m => RadixTree (Code m a) -> Code m (RadixTree a)
sequenceCode = sequenceCode0
