{-# LANGUAGE BangPatterns
           , OverloadedLists
           , OverloadedStrings #-}

module Test.RadixNTree.Word8.Key
  ( test
  ) where

import           Data.RadixTree.Word8.Key        as Radix
import           Data.RadixTree.Word8.Key.Unsafe as Radix
import           Data.Radix1Tree.Word8.Key        as Radix1
import           Data.Radix1Tree.Word8.Key.Unsafe as Radix1

import qualified Data.ByteString.Lazy.Internal as LazyBS (ByteString (..))
import           Data.String
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Primitive.ByteArray as Prim
import qualified Data.Text.Array as Array
import qualified Data.Text.Internal as Strict (Text (..))
import qualified Data.Text.Internal.Lazy as LazyText (Text (..))
import           Data.Word
import           Test.Hspec



buildRef :: Build
buildRef = Build (Snoc (Snoc (Snoc Lin [0xC2]) [0xA3, 0x24, 0xE2]) [0x82, 0xAC])

buildRef1 :: Build1
buildRef1 = Build1 $ ((\(Build x) -> x) buildRef) :/ [0xC2, 0xA4]



rawRef, rawRef1, utf8Ref, utf8Ref1 :: IsString a => a
rawRef   = "\xC2\xA3$\xE2\x82\xAC"
rawRef1  = "\xC2\xA3$\xE2\x82\xAC\xC2\xA4"
utf8Ref  = "£$€"
utf8Ref1 = "£$€¤"



feedRef :: [Word8]
feedRef = [0xC2, 0xA3, 0x24, 0xE2, 0x82, 0xAC]

feedRef1 :: NonEmpty Word8
feedRef1 = 0xC2 :| [0xA3, 0x24, 0xE2, 0x82, 0xAC, 0xC2, 0xA4]

destroy :: Feed -> [Word8]
destroy (Feed feed) =
  feed $ \step ->

    let go s =
          case step s of
            More w s' -> w : go s'
            Done      -> []

    in go

destroy1 :: Feed1 -> NonEmpty Word8
destroy1 (Feed1 w feed) = w :| destroy (Feed feed)



test :: Spec
test = do
  describe "build" $ do
    it "bytes" $
      Radix.buildBytes buildRef `shouldBe` feedRef

    it "bytes/1" $
      Radix1.buildBytes buildRef1 `shouldBe` feedRef1

    it "ByteString" $
      Radix.buildByteString buildRef `shouldBe` rawRef

    it "ByteString/1" $
      Radix1.buildByteString buildRef1 `shouldBe` rawRef1

    it "ShortByteString" $
      Radix.buildShortByteString buildRef `shouldBe` rawRef

    it "ShortByteString/1" $
      Radix1.buildShortByteString buildRef1 `shouldBe` rawRef1

    it "Text" $
      Radix.unsafeBuildText buildRef `shouldBe` utf8Ref

    it "Text/1" $
      Radix1.unsafeBuildText buildRef1 `shouldBe` utf8Ref1

  describe "feed" $ do
    it "bytes" $
      destroy (Radix.feedBytes feedRef) `shouldBe` feedRef

    it "bytes/1" $
      destroy1 (Radix1.feedBytes feedRef1) `shouldBe` feedRef1

    it "ByteString" $
      destroy (Radix.feedByteString rawRef) `shouldBe` feedRef

    it "ByteString/1" $
      destroy1 (Radix1.unsafeFeedByteString rawRef1) `shouldBe` feedRef1

    it "ShortByteString" $
      destroy (Radix.feedShortByteString rawRef) `shouldBe` feedRef

    it "ShortByteString/1" $
      destroy1 (Radix1.unsafeFeedShortByteString rawRef1) `shouldBe` feedRef1

    it "Text" $
      destroy (Radix.feedText utf8Ref) `shouldBe` feedRef

    it "Text/1" $
      destroy1 (Radix1.unsafeFeedText utf8Ref1) `shouldBe` feedRef1

    it "lazy ByteString" $
      let ref = LazyBS.Chunk [0xC2] . LazyBS.Chunk [0xA3, 0x24, 0xE2]
              $ LazyBS.Chunk [0x82, 0xAC] LazyBS.Empty

      in destroy (Radix.feedLazyByteString ref) `shouldBe` feedRef

    it "lazy ByteString/1" $
      let rest = LazyBS.Chunk [0xA3, 0x24, 0xE2]
               . LazyBS.Chunk [0x82, 0xAC]
               $ LazyBS.Chunk [0xC2, 0xA4] LazyBS.Empty

      in destroy1 (Radix1.unsafeFeedLazyByteString "\xC2" rest) `shouldBe` feedRef1

    it "lazy Text" $
      let !(Prim.ByteArray c1) = [0xC2]
          !(Prim.ByteArray c2) = [0xA3, 0x24, 0xE2]
          !(Prim.ByteArray c3) = [0x82, 0xAC]

          ref = LazyText.Chunk (Strict.Text (Array.ByteArray c1) 0 1)
              . LazyText.Chunk (Strict.Text (Array.ByteArray c2) 0 3)
              . LazyText.Chunk (Strict.Text (Array.ByteArray c3) 0 2)
              $ LazyText.Empty

      in destroy (Radix.feedLazyText ref) `shouldBe` feedRef

    it "lazy Text/1" $
      let !(Prim.ByteArray c1) = [0xC2]
          !(Prim.ByteArray c2) = [0xA3, 0x24, 0xE2]
          !(Prim.ByteArray c3) = [0x82, 0xAC]
          !(Prim.ByteArray c4) = [0xC2, 0xA4]

          first = Strict.Text (Array.ByteArray c1) 0 1

          ref = LazyText.Chunk (Strict.Text (Array.ByteArray c2) 0 3)
              . LazyText.Chunk (Strict.Text (Array.ByteArray c3) 0 2)
              . LazyText.Chunk (Strict.Text (Array.ByteArray c4) 0 2)
              $ LazyText.Empty

      in destroy1 (Radix1.unsafeFeedLazyText first ref) `shouldBe` feedRef1
