module Data.RadixNTree.Word8.Conversion
  ( Lazy.LazyRadixTree
  , Lazy.LazyRadix1Tree
  , toLazy0
  , toLazy1

  , Strict.StrictRadixTree
  , Strict.StrictRadix1Tree
  , toStrict0
  , toStrict1
  ) where

import qualified Data.RadixNTree.Word8.Lazy as Lazy
import qualified Data.RadixNTree.Word8.Strict as Strict



toLazy0 :: Strict.StrictRadixTree a -> Lazy.LazyRadixTree a
toLazy0 (Strict.RadixTree mx t) = Lazy.RadixTree mx (toLazy1 t)

toLazy1 :: Strict.StrictRadix1Tree a -> Lazy.LazyRadix1Tree a
toLazy1 t =
  case t of
    Strict.Bin p l r     -> Lazy.Bin p (toLazy1 l) (toLazy1 r)
    Strict.Tip arr mx dx -> Lazy.Tip arr mx (toLazy1 dx)
    Strict.Nil           -> Lazy.Nil



toStrict0 :: Lazy.LazyRadixTree a -> Strict.StrictRadixTree a
toStrict0 (Lazy.RadixTree mx t) = Strict.RadixTree mx (toStrict1 t)

toStrict1 :: Lazy.LazyRadix1Tree a -> Strict.StrictRadix1Tree a
toStrict1 t =
  case t of
    Lazy.Bin p l r     -> Strict.Bin p (toStrict1 l) (toStrict1 r)
    Lazy.Tip arr mx dx -> Strict.Tip arr mx (toStrict1 dx)
    Lazy.Nil           -> Strict.Nil
