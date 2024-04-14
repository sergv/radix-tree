{-# LANGUAGE TemplateHaskellQuotes #-}

{-|
    Template Haskell helper functions.
 -}

module Data.Patricia.Word.Strict.TH where

import           Data.Patricia.Word.Strict.Internal

import           Language.Haskell.TH.Syntax



-- | \(\mathcal{O}(n)\).
--   Evaluate a tree of typed expressions.
sequenceCode :: Quote m => Patricia (Code m a) -> Code m (Patricia a)
sequenceCode t =
  case t of
    Bin p l r ->
      [|| Bin
            p
            $$(sequenceCode l)
            $$(sequenceCode r)
       ||]

    Tip k a     -> [|| Tip k $$(a) ||]
    Nil         -> [|| Nil ||]
