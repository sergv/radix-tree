{-|
    Safe functions for building and destroying non-empty radix tree keys.
 -}

module Data.Radix1Tree.Word8.Key
  ( -- * Build
    Build1

    -- ** Raw
  , buildBytes

    -- ** ByteString
  , buildByteString
  , buildShortByteString

    -- ** Text
    -- | See "Data.Radix1Tree.Word8.Key.Unsafe#g:build.text".

    -- * Feed
  , Feed1

    -- ** Raw
  , feedBytes

    -- ** ByteString
    -- | See "Data.Radix1Tree.Word8.Key.Unsafe#g:feed.bytestring".

    -- ** Text
    -- | See "Data.Radix1Tree.Word8.Key.Unsafe#g:feed.text".
  ) where

import           Data.RadixNTree.Word8.Key

import qualified Data.ByteString as Strict (ByteString)
import           Data.ByteString.Short (ShortByteString)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Word



-- | Convert the key into a non-empty list of bytes.
buildBytes :: Build1 -> NonEmpty Word8
buildBytes = buildBytes1

-- | Convert the key into a non-empty strict 'Strict.ByteString'.
buildByteString :: Build1 -> Strict.ByteString
buildByteString = buildByteString1

-- | Convert the key into a non-empty 'ShortByteString'.
buildShortByteString :: Build1 -> ShortByteString
buildShortByteString = buildShortByteString1



{-# INLINE feedBytes #-}
-- | Convert the non-empty list of bytes into a key.
feedBytes :: NonEmpty Word8 -> Feed1
feedBytes = feedBytes1
