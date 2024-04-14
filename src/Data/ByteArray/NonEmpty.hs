{-# LANGUAGE BangPatterns
           , RankNTypes
           , ScopedTypeVariables
           , UnboxedTuples #-}

module Data.ByteArray.NonEmpty
  ( Step (..)

  , fromStep
  , toNonEmpty
  , toList

  , dropByteArray

  , appendByteArray
  , dropAppendByteArray
  , fromStepAppend

  , splitByteArray
  ) where

import           Control.Monad.ST
import           Data.Primitive.ByteArray
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Word



-- | Single step of destroying a key.
data Step a b = More a b
              | Done

{-# INLINE fromStep #-}
fromStep :: (x -> Step Word8 x) -> Word8 -> x -> ByteArray
fromStep (more :: x -> Step Word8 x) = \w0 -> go 1 (\marr -> writeByteArray marr 0 w0)
  where
    go :: Int -> (forall s. MutableByteArray s -> ST s ()) -> x -> ByteArray
    go !n write s =
      case more s of
        More w s' ->
          let write' marr = do
                write marr
                writeByteArray marr n w

          in go (n + 1) write' s'

        Done      ->
          runST $ do
            marr <- newByteArray n
            write marr
            unsafeFreezeByteArray marr



{-# INLINE toNonEmpty #-}
toNonEmpty :: ByteArray -> NonEmpty Word8
toNonEmpty arr = indexByteArray arr 0 :| toListFrom 1 arr

{-# INLINE toList #-}
toList :: ByteArray -> [Word8]
toList = toListFrom 0

{-# INLINE toListFrom #-}
toListFrom :: Int -> ByteArray -> [Word8]
toListFrom n0 arr = go n0
  where
    go n
      | n >= sizeofByteArray arr = []
      | otherwise                = indexByteArray arr n : go (n + 1)



dropByteArray :: Int -> ByteArray -> ByteArray
dropByteArray n arr =
  runST $ do
    let len = sizeofByteArray arr - n
    mbrr <- newByteArray len
    copyByteArray mbrr 0 arr n len
    unsafeFreezeByteArray mbrr



appendByteArray :: ByteArray -> ByteArray -> ByteArray
appendByteArray arr brr =
  runST $ do
    let alen = sizeofByteArray arr
        blen = sizeofByteArray brr
    mcrr <- newByteArray (alen + blen)
    copyByteArray mcrr 0    arr 0 alen
    copyByteArray mcrr alen brr 0 blen
    unsafeFreezeByteArray mcrr



dropAppendByteArray :: Int -> ByteArray -> ByteArray -> ByteArray
dropAppendByteArray n arr brr =
  runST $ do
    let alen = sizeofByteArray arr - n
        blen = sizeofByteArray brr
    mcrr <- newByteArray (alen + blen)
    copyByteArray mcrr 0    arr n alen
    copyByteArray mcrr alen brr 0 blen
    unsafeFreezeByteArray mcrr



{-# INLINE fromStepAppend #-}
fromStepAppend :: (x -> Step Word8 x) -> Word8 -> x -> ByteArray -> ByteArray
fromStepAppend (more :: x -> Step Word8 x) = \w0 s0 arr ->
  let go :: Int -> (forall s. MutableByteArray s -> ST s ()) -> x -> ByteArray
      go !n write s =
        case more s of
          More w s' ->
            let write' mbrr = do
                  writeByteArray mbrr n w
                  write mbrr

            in go (n + 1) write' s'

          Done      ->
            runST $ do
              let alen = sizeofByteArray arr
              mbrr <- newByteArray (n + alen)
              write mbrr
              copyByteArray mbrr n arr 0 alen
              unsafeFreezeByteArray mbrr

  in go 1 (\mbrr -> writeByteArray mbrr 0 w0) s0



data Wrap = Wrap {-# UNPACK #-} !ByteArray {-# UNPACK #-} !ByteArray

splitByteArray :: Int -> Int -> ByteArray -> (# ByteArray, ByteArray #)
splitByteArray offset n arr =
  let f = runST $ do
            let alen = sizeofByteArray arr

            mbrr <- newByteArray n
            copyByteArray mbrr 0 arr offset n
            brr <- unsafeFreezeByteArray mbrr

            let clen = alen - n

            mcrr <- newByteArray clen
            copyByteArray mcrr 0 arr n clen
            crr <- unsafeFreezeByteArray mcrr

            pure $ Wrap brr crr

  in case f of
       Wrap brr crr -> (# brr, crr #)
