{-|
    Safe functions for building and destroying radix tree keys.
 -}

module Data.RadixTree.Word8.Key
  ( -- * Build
    Build

    -- ** Raw
  , buildBytes

    -- ** ByteString
  , buildByteString
  , buildShortByteString

    -- ** Text
    -- | See "Data.RadixTree.Word8.Key.Unsafe#g:build/text".

    -- * Feed
  , Feed

    -- ** Raw
  , feedBytes

    -- ** ByteString
  , feedByteString
  , feedShortByteString
  , feedLazyByteString

    -- ** Text
  , feedText
  , feedLazyText
  ) where

import           Data.RadixNTree.Word8.Key

import qualified Data.ByteString as Strict (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import           Data.ByteString.Short (ShortByteString)
import qualified Data.Text as Strict (Text)
import qualified Data.Text.Lazy as Lazy (Text)
import           Data.Word



-- | Convert a key into a list of bytes.
buildBytes :: Build -> [Word8]
buildBytes = buildBytes0

-- | Convert a key into a strict 'Strict.ByteString'.
buildByteString :: Build -> Strict.ByteString
buildByteString = buildByteString0

-- | Convert a key into a 'ShortByteString'.
buildShortByteString :: Build -> ShortByteString
buildShortByteString = buildShortByteString0



{-# INLINE feedBytes #-}
-- | Convert a list of bytes into a key.
feedBytes :: [Word8] -> Feed
feedBytes = feedBytes0

{-# INLINE feedByteString #-}
-- | Convert a strict 'Strict.ByteString' into a key.
feedByteString :: Strict.ByteString -> Feed
feedByteString = feedByteString0

{-# INLINE feedShortByteString #-}
-- | Convert a 'ShortByteString' into a key.
feedShortByteString :: ShortByteString -> Feed
feedShortByteString = feedShortByteString0

{-# INLINE feedLazyByteString #-}
-- | Convert a lazy 'Lazy.ByteString' into a key.
feedLazyByteString :: Lazy.ByteString -> Feed
feedLazyByteString = feedLazyByteString0



{-# INLINE feedText #-}
-- | Convert a strict 'Strict.Text' into a key.
feedText :: Strict.Text -> Feed
feedText = feedText0

{-# INLINE feedLazyText #-}
-- | Convert a lazy 'Lazy.Text' into a key.
feedLazyText :: Lazy.Text -> Feed
feedLazyText = feedLazyText0
