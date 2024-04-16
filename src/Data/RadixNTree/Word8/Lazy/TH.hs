{-# LANGUAGE TemplateHaskellQuotes #-}

module Data.RadixNTree.Word8.Lazy.TH
  ( RadixTree
  , sequenceCode0

  , Radix1Tree
  , sequenceCode1
  ) where

import           Data.RadixNTree.Word8.Lazy

import           Language.Haskell.TH.Syntax



sequenceCode0 :: Quote m => RadixTree (Code m a) -> Code m (RadixTree a)
sequenceCode0 (RadixTree mx t) =
  [|| RadixTree $$(sequenceMaybe mx) $$(sequenceCode1 t) ||]

sequenceCode1 :: Quote m => Radix1Tree (Code m a) -> Code m (Radix1Tree a)
sequenceCode1 t =
  case t of
    Bin p l r     ->
      [|| Bin
            p
            $$(sequenceCode1 l)
            $$(sequenceCode1 r)
       ||]

    Tip arr mx dx -> [|| Tip arr $$(sequenceMaybe mx) $$(sequenceCode1 dx) ||]

    Nil           -> [|| Nil ||]



sequenceMaybe :: Quote m => Maybe (Code m a) -> Code m (Maybe a)
sequenceMaybe mx =
  case mx of
    Just x  -> [|| Just $$(x) ||]
    Nothing -> [|| Nothing ||]
