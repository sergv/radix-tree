----------------------------------------------------------------------------
-- |
-- Module      :  TestMain
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS

import Data.Char
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.RadixTree (RadixTree)
import qualified Data.RadixTree as RT
import Data.Word

import Test.QuickCheck
import Test.QuickCheck.Poly
import Test.Tasty
import Test.Tasty.QuickCheck as QC

newtype AsciiChar = AsciiChar { unAsciiChar :: Char }

instance Arbitrary AsciiChar where
  arbitrary = AsciiChar <$> choose ('a', 'z')
  shrink (AsciiChar 'a') = []
  shrink (AsciiChar c)   = [AsciiChar c' | c' <- ['a'..pred c]]

mkAsciiChar :: Word8 -> AsciiChar
mkAsciiChar = AsciiChar . chr. fromIntegral

asciiByte :: AsciiChar -> Word8
asciiByte = fromIntegral . ord . unAsciiChar

instance Arbitrary ShortByteString where
  arbitrary =
    BSS.pack . map asciiByte <$> listOf arbitrary
  shrink =
    map (BSS.pack . map asciiByte) . shrink . map mkAsciiChar . BSS.unpack

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
qcProps = adjustOption (\(QuickCheckTests n) -> QuickCheckTests (max 10000 n)) $ testGroup "radix tree"
  [ QC.testProperty "∀ t: RT.lookup k (RT.insert k v t) == v" $
    \(t :: RadixTree A) (k :: ShortByteString) (v :: A) ->
      RT.lookup k (RT.insert k v t) == Just v
  , QC.testProperty "∀ t: RT.lookup k (RT.insert k v2 (RT.insert k v1 t)) == v2" $
    \(t :: RadixTree A) (k :: ShortByteString) (v1 :: A) (v2 :: A) ->
      RT.lookup k (RT.insert k v2 (RT.insert k v1 t)) == Just v2

  , QC.testProperty "∀ xs: RT.fromList xs == M.fromList xs" $
    \(xs :: [(ShortByteString, A)]) ->
      RT.toAscList (RT.fromList xs) == M.toAscList (M.fromList xs)

  , QC.testProperty "∀ xs: RT.size (RT.fromList xs) == M.size (M.fromList xs)" $
    \(xs :: [(ShortByteString, A)]) ->
      RT.size (RT.fromList xs) == M.size (M.fromList xs)

  , QC.testProperty "∀ f: RT.mapMaybe f == M.mapMaybe f" $
    \(f :: Fun A (Maybe B)) ->
      RT.mapMaybe (applyFun f) ==== M.mapMaybe (applyFun f)

  , QC.testProperty "∀ k v t: RT.insert k v t == M.insert k v t" $
    \(k :: ShortByteString) (v :: A) ->
      RT.insert k v ==== M.insert k v

  , QC.testProperty "∀ f xs ys: RT.mergeWith f xs ys == M.mergeWith f xs ys" $
    \(f :: Fun (A, A) A) ->
      RT.unionWith (curry (applyFun f)) ===== M.unionWith (curry (applyFun f))
  ]

(====)
  :: Eq b
  => (RadixTree a -> RadixTree b)
  -> (Map ShortByteString a -> Map ShortByteString b)
  -> [(ShortByteString, a)]
  -> Bool
(====) f g xs =
  RT.toAscList (f (RT.fromList xs)) == M.toAscList (g (M.fromList xs))

(=====)
  :: Eq a
  => (RadixTree a -> RadixTree a -> RadixTree a)
  -> (Map ShortByteString a -> Map ShortByteString a -> Map ShortByteString a)
  -> [(ShortByteString, a)]
  -> [(ShortByteString, a)]
  -> Bool
(=====) f g xs ys =
  RT.toAscList (f (RT.fromList xs) (RT.fromList ys)) == M.toAscList (g (M.fromList xs) (M.fromList ys))

-- unitTests :: TestTree
-- unitTests = testGroup "Unit tests"
--   [ testCase "List comparison (different length)" $
--       [1, 2, 3] `compare` [1,2] @?= GT
--
--   -- the following test does not hold
--   , testCase "List comparison (same length)" $
--       [1, 2, 3] `compare` [1,2,2] @?= LT
--   ]
