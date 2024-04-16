{-# OPTIONS_HADDOCK not-home #-}

{-|
    Radix tree key internals, and unsafe functions for building and destroying them.
 -}

module Data.RadixTree.Word8.Key.Unsafe
  ( -- * Build
    Build (..)
  , Tsil (..)

    -- ** Text #build/text#
  , unsafeBuildText

    -- * Feed
  , Feed (..)
  , Step (..)
  ) where

import           Data.ByteArray.NonEmpty (Step (..))
import           Data.RadixNTree.Word8.Key

import qualified Data.Text as Strict (Text)



-- | Convert a key into a strict 'Strict.Text'.
--
--   No checks are made to ensure the resulting value is a valid sequence
--   of UTF-8 code units.
unsafeBuildText :: Build -> Strict.Text
unsafeBuildText = unsafeBuildText0
