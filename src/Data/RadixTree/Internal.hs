----------------------------------------------------------------------------
-- |
-- Module      :  Data.RadixTree.Internal
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.RadixTree.Internal
  ( RadixTree(..)
  , empty
  , insert
  , lookup
  , fromList
  , toList
  , toAscList
  ) where

import Prelude hiding (lookup)

import Control.Arrow (first)
import Control.DeepSeq
import Control.Monad.ST
import Control.Monad.ST.Unsafe

import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Short.Internal as BSSI
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.List as L
import Data.Primitive.ByteArray
import Data.Word
import GHC.Generics (Generic)

-- | A tree data structure that efficiently indexes values by string keys.
data RadixTree a
  = RadixNode
      !(Maybe a)
      !(IntMap (RadixTree a)) -- ^ Either has 0 or 2 or more children, never 1.
  | RadixStr
      !(Maybe a)
      {-# UNPACK #-} !ShortByteString -- ^ Non-empty
      !(RadixTree a)
  deriving (Show, Functor, Foldable, Traversable, Generic)

instance NFData a => NFData (RadixTree a)

-- -- | Path within a RadixTree.
-- data RadixPath
--   = Here
--   | SkipRadixStr !RadixPath
--   | Dive !Word8 !RadixPath
--   deriving (Eq, Ord, Show, Generic)

empty :: RadixTree a
empty = RadixNode Nothing IM.empty

splitShortByteString :: Int -> ShortByteString -> (ShortByteString, ShortByteString, Word8, ShortByteString)
splitShortByteString n (BSSI.SBS source) = runST $ do
  prefix    <- newByteArray prefixSize
  midSuffix <- newByteArray midSuffixSize
  suffix    <- newByteArray suffixSize
  copyByteArray prefix    0 source' 0       prefixSize
  copyByteArray midSuffix 0 source' n       midSuffixSize
  copyByteArray suffix    0 source' (n + 1) suffixSize
  ByteArray prefix#    <- unsafeFreezeByteArray prefix
  -- TODO: benchmark unsafeInterleaveST here
  ByteArray midSuffix# <- unsafeInterleaveST $ unsafeFreezeByteArray midSuffix
  ByteArray suffix#    <- unsafeInterleaveST $ unsafeFreezeByteArray suffix
  pure (BSSI.SBS prefix#, BSSI.SBS midSuffix#, indexByteArray source' n, BSSI.SBS suffix#)
  where
    source' = ByteArray source
    prefixSize = n
    midSuffixSize = sizeofByteArray source' - prefixSize
    suffixSize = midSuffixSize - 1

dropShortByteString :: Int -> ShortByteString -> ShortByteString
dropShortByteString !n (BSSI.SBS source) = runST $ do
  dest <- newByteArray size
  copyByteArray dest 0 source' n size
  ByteArray dest# <- unsafeFreezeByteArray dest
  pure $ BSSI.SBS dest#
  where
    source' = ByteArray source
    !size = sizeofByteArray source' - n

data Mismatch
  = IsPrefix
  | CommonPrefixThenMismatch
      !ShortByteString -- ^ Prefix of node contents common with the key
      ShortByteString -- ^ Suffix with the first mismatching byte
      Word8           -- ^ First byte of the suffix
      ShortByteString -- ^ Rest of node contents, suffix
  deriving (Show, Generic)

-- instance Pretty Mismatch where
--   pretty = ppGeneric

-- indexByteArraySafe
--   :: ByteArray
--   -> Int
--   -> Word8
-- indexByteArraySafe xs i
--   | i < sizeofByteArray xs = indexByteArray xs i
--   | otherwise              = error $ "Index is out of bounds: " ++ show i

analyseMismatch
  :: ShortByteString -- ^ Key
  -> Int             -- ^ Key offset
  -> ShortByteString -- ^ Node contents
  -> Mismatch
analyseMismatch (BSSI.SBS key) !keyOffset nodeContentsBS@(BSSI.SBS nodeContents) =
  -- case L.find (\i -> (indexByteArray key' i :: Word8) /= indexByteArray nodeContents' i) [0 .. min keyLeft contentsSize] of
  case findMismatch 0 of
    Nothing          -> IsPrefix
    Just mismatchIdx ->
      case splitShortByteString mismatchIdx nodeContentsBS of
        (prefix, midSuffix, mid, suffix) -> CommonPrefixThenMismatch prefix midSuffix mid suffix
  where
    keySize      = sizeofByteArray key'
    keyLeft      = keySize - keyOffset
    contentsSize = sizeofByteArray nodeContents'

    key'          = ByteArray key
    nodeContents' = ByteArray nodeContents

    limit :: Int
    limit = min keyLeft contentsSize

    findMismatch :: Int -> Maybe Int
    findMismatch !i
      | i == limit
      = if i == contentsSize
        then Nothing
        else Just i -- Key ended in the middle of node's packed key.
      | (indexByteArray key' (keyOffset + i) :: Word8) == indexByteArray nodeContents' i
      = findMismatch $ i + 1
      | otherwise
      = Just i

mkRadixStr :: ShortByteString -> RadixTree a -> RadixTree a
mkRadixStr str rest
  | BSS.null str = rest
  | otherwise    = RadixStr Nothing str rest

insert :: forall a. ShortByteString -> a -> RadixTree a -> RadixTree a
insert = insert'

{-# INLINE insert' #-}
insert' :: forall a. ShortByteString -> a -> RadixTree a -> RadixTree a
insert' key value = go 0
  where
    len = BSS.length key

    readKey :: Int -> Int
    readKey = fromIntegral . BSSI.unsafeIndex key

    go :: Int -> RadixTree a -> RadixTree a
    go i
      | i < len
      = \case
        RadixNode oldValue children
          | IM.null children ->
            RadixStr oldValue (dropShortByteString i key) $ RadixNode (Just value) IM.empty
          | otherwise ->
            RadixNode oldValue $
            IM.alter (Just . maybe optNode (go i')) c children
          where
            c :: Int
            c = readKey i
            i' = i + 1
            optNode =
              mkRadixStr (dropShortByteString i' key) $ RadixNode (Just value) IM.empty
        RadixStr oldValue packedKey rest ->
          case analyseMismatch key i packedKey of
            IsPrefix ->
              RadixStr oldValue packedKey $ go (i + BSS.length packedKey) rest
            CommonPrefixThenMismatch prefix midSuffix mid suffix ->
              (if BSS.null prefix then id else RadixStr oldValue prefix) $
                if isKeyEnded
                then
                  RadixStr (Just value) midSuffix rest
                else
                  RadixNode (if BSS.null prefix then oldValue else Nothing) $
                  IM.fromList
                    [ ( mid'
                      , mkRadixStr suffix rest
                      )
                    , ( readKey i'
                      , mkRadixStr (dropShortByteString (i' + 1) key) $ RadixNode (Just value) IM.empty
                      )
                    ]
              where
                i'         = i + BSS.length prefix
                isKeyEnded = i' >= len
                mid'       = fromIntegral mid
      | otherwise
      = \case
        RadixNode _ children -> RadixNode (Just value) children
        RadixStr _ key' rest -> RadixStr (Just value) key' rest

canStripPrefixFromShortByteString
  :: Int -> ShortByteString -> ShortByteString -> Bool
canStripPrefixFromShortByteString bigStart (BSSI.SBS small) (BSSI.SBS big)
  | bigStart + smallSize > bigSize = False
  | otherwise                      = findMismatch 0
  where
    small' = ByteArray small
    big'   = ByteArray big

    smallSize = sizeofByteArray small'
    bigSize   = sizeofByteArray big'

    findMismatch :: Int -> Bool
    findMismatch !i
      | i == smallSize
      = True
      | (indexByteArray small' i :: Word8) == indexByteArray big' (bigStart + i)
      = findMismatch $ i + 1
      | otherwise
      = False

lookup :: forall a. ShortByteString -> RadixTree a -> Maybe a
lookup key = go 0
  where
    len = BSS.length key

    readKey :: Int -> Int
    readKey = fromIntegral . BSSI.unsafeIndex key

    go :: Int -> RadixTree a -> Maybe a
    go !n tree
      | n == len
      = case tree of
        RadixNode val _  -> val
        RadixStr val _ _ -> val
      | otherwise
      = case tree of
      RadixNode _ children      ->
        IM.lookup (readKey n) children >>= go (n + 1)
      RadixStr _ packedKey rest
        | canStripPrefixFromShortByteString n packedKey key
        -> go (n + BSS.length packedKey) rest
        | otherwise
        -> Nothing

fromList :: [(ShortByteString, a)] -> RadixTree a
fromList =
  L.foldl' (\acc (k, v) -> insert' k v acc) empty

toList :: RadixTree a -> [(ShortByteString, a)]
toList = toAscList

toAscList :: forall a. RadixTree a -> [(ShortByteString, a)]
toAscList = map (first BSS.pack) . go
  where
    go :: RadixTree a -> [([Word8], a)]
    go = \case
      RadixNode val children ->
        maybe id (\val' ys -> ([], val') : ys) val $
        concatMap (\(c, child) -> map (first (fromIntegral c :)) $ go child) $
        IM.toAscList children
      RadixStr val packedKey rest ->
        maybe id (\val' ys -> ([], val') : ys) val $
        map (first (BSS.unpack packedKey ++)) $
        go rest
