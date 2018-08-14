----------------------------------------------------------------------------
-- |
-- Module      :  TestMain
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module TestMain (main) where

import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS

import Data.Char
import qualified Data.Map.Strict as M
import Data.RadixTree (RadixTree)
import qualified Data.RadixTree as RT

import Test.QuickCheck
import Test.QuickCheck.Poly
import Test.Tasty
import Test.Tasty.QuickCheck as QC

instance Arbitrary ShortByteString where
  arbitrary = BSS.pack . map (fromIntegral. ord) <$> listOf (choose ('a', 'z')) -- arbitrary
  shrink = map BSS.pack . shrink . BSS.unpack


instance Arbitrary a => Arbitrary (RadixTree a) where
  arbitrary = RT.fromList <$> arbitrary
  shrink = map RT.fromList . shrink . RT.toAscList

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps :: TestTree
qcProps =  testGroup "radix tree"
  [ QC.testProperty "∀ t: RT.lookup k (RT.insert k v t) == v" $
    \(t :: RadixTree A) (k :: ShortByteString) (v :: A) ->
      RT.lookup k (RT.insert k v t) == Just v
  , QC.testProperty "∀ t: RT.lookup k (RT.insert k v2 (RT.insert k v1 t)) == v2" $
    \(t :: RadixTree A) (k :: ShortByteString) (v1 :: A) (v2 :: A) ->
      RT.lookup k (RT.insert k v2 (RT.insert k v1 t)) == Just v2

  , QC.testProperty "∀ xs: RT.fromList xs == M.fromList xs" $
    \(xs :: [(ShortByteString, A)]) ->
      RT.toAscList (RT.fromList xs) == M.toAscList (M.fromList xs)

  , QC.testProperty "∀ t: RT.insert k v t == M.insert k v t" $
    \(xs :: [(ShortByteString, A)]) (k :: ShortByteString) (v :: A) ->
      RT.toAscList (RT.insert k v (RT.fromList xs)) == M.toAscList (M.insert k v (M.fromList xs))
  ]

-- unitTests :: TestTree
-- unitTests = testGroup "Unit tests"
--   [ testCase "List comparison (different length)" $
--       [1, 2, 3] `compare` [1,2] @?= GT
--
--   -- the following test does not hold
--   , testCase "List comparison (same length)" $
--       [1, 2, 3] `compare` [1,2,2] @?= LT
--   ]
