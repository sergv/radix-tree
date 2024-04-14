module Main where

import qualified Test.Patricia.Word.Lazy as Pat.Lazy
import qualified Test.Patricia.Word.Strict as Pat.Strict
import qualified Test.RadixTree.Word8.Lazy as Radix.Lazy
import qualified Test.RadixTree.Word8.Strict as Radix.Strict
import qualified Test.RadixNTree.Word8.Key as Radix.Key
import qualified Test.Zebra.Word as Zebra

import           Test.Hspec



main :: IO ()
main =
  hspec $ do
    describe "Patricia/Lazy" $
      Pat.Lazy.test

    describe "Patricia/Strict" $
      Pat.Strict.test

    describe "RadixNTree/Key" $
      Radix.Key.test

    describe "RadixTree/Lazy" $
      Radix.Lazy.test

    describe "RadixTree/Strict" $
      Radix.Strict.test

    describe "Zebra" $
      Zebra.test
