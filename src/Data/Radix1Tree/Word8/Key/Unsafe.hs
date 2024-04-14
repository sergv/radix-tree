{-# OPTIONS_HADDOCK not-home #-}

{-|
    Non-empty radix tree key internals,
    and unsafe functions for building and destroying them.
 -}

module Data.Radix1Tree.Word8.Key.Unsafe
  ( -- * Build
    Build1 (..)
  , YtpmeNon (..)
  , Tsil (..)

    -- ** Text #build.text#
  , unsafeBuildText

    -- * Feed
  , Feed1 (..)
  , Step (..)

    -- ** ByteString #feed.bytestring#
  , unsafeFeedByteString
  , unsafeFeedShortByteString
  , unsafeFeedLazyByteString

    -- ** Text #feed.text#
  , unsafeFeedText
  , unsafeFeedLazyText
  ) where

import           Data.ByteArray.NonEmpty (Step (..))
import           Data.RadixNTree.Word8.Key

import qualified Data.ByteString as Strict (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import           Data.ByteString.Short (ShortByteString)
import qualified Data.Text as Strict (Text)
import qualified Data.Text.Lazy as Lazy (Text)



-- | Convert a key into a non-empty strict 'Strict.Text'.
--
--   No checks are made to ensure the resulting value is a valid sequence
--   of UTF-8 code units.
unsafeBuildText :: Build1 -> Strict.Text
unsafeBuildText = unsafeBuildText1



{-# INLINE unsafeFeedByteString #-}
-- | Convert a strict 'Strict.ByteString' into a key.
--
--   The 'Strict.ByteString' is assumed to be non-empty.
unsafeFeedByteString :: Strict.ByteString -> Feed1
unsafeFeedByteString = unsafeFeedByteString1

{-# INLINE unsafeFeedShortByteString #-}
-- | Convert a 'ShortByteString' into a key.
--
--   The 'ShortByteString' is assumed to be non-empty.
unsafeFeedShortByteString :: ShortByteString -> Feed1
unsafeFeedShortByteString = unsafeFeedShortByteString1

{-# INLINE unsafeFeedLazyByteString #-}
-- | Convert a lazy 'Lazy.ByteString', in the form of the first chunk plus the rest,
--   into a key.
--
--   The first chunk is assumed to be non-empty.
unsafeFeedLazyByteString :: Strict.ByteString -> Lazy.ByteString -> Feed1
unsafeFeedLazyByteString = unsafeFeedLazyByteString1



{-# INLINE unsafeFeedText #-}
-- | Convert a strict 'Strict.Text' into a key.
--
--   The 'Strict.Text' is assumed to be non-empty.
unsafeFeedText :: Strict.Text -> Feed1
unsafeFeedText = unsafeFeedText1

{-# INLINE unsafeFeedLazyText #-}
-- | Convert a lazy 'Lazy.Text', in the form of the first chunk plus the rest,
--   into a key.
--
--   The first chunk is assumed to be non-empty.
unsafeFeedLazyText :: Strict.Text -> Lazy.Text -> Feed1
unsafeFeedLazyText = unsafeFeedLazyText1
