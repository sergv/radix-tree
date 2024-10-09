{-# LANGUAGE BangPatterns
           , DeriveLift
           , RankNTypes
           , ScopedTypeVariables #-}

module Data.RadixNTree.Word8.Strict.Pointer
  ( Pointer (..)
  , pointer0
  , pointer1

  , follow0
  , follow1
  ) where

import           Data.ByteArray.NonEmpty
import           Data.RadixNTree.Word8.Key
import           Data.RadixNTree.Word8.Strict

import           Control.Monad.ST
import           Data.Bits
import           Data.Primitive.ByteArray
import           Data.Word
import           Language.Haskell.TH.Syntax



-- | Pure compressed tree reference.
--
--   @since 1.1
data Pointer = Pointer
                 {-# UNPACK #-} !Word -- ^ Node depth (0 is root).
                 !ByteArray           -- ^ Little-endian bitmask of size @depth@.
                                      --   'Bin' choices are represented as 0 and 1 for
                                      --   left and right respectively;
                                      --   'Tip's can hold any data.
               deriving (Show, Lift)



-- | Mark a bit at the given depth as @1@.
mark :: MutableByteArray s -> Word -> ST s ()
mark marr n = do
  let x = fromIntegral $ n `unsafeShiftR` 3

      y = fromIntegral $ n .&. 0x07

  i <- readByteArray marr x

  let i' = i .|. unsafeShiftL 1 y

  writeByteArray marr x (i' :: Word8)

-- | Check if the bit at the given depth is @0@.
left :: ByteArray -> Word -> Bool
left arr n =
  let x = fromIntegral $ n `unsafeShiftR` 3

      y = fromIntegral $ n .&. 0x07

  in (unsafeShiftR (indexByteArray arr x :: Word8) y) .&. 0x1 == 0

-- | Create a bitmask that can hold @depth@ bits and populate it.
form :: (forall s. MutableByteArray s -> ST s ()) -> Word -> ByteArray
form go n = do
  runST $ do
    let m = fromIntegral (n `unsafeShiftR` 3) + 1
    marr <- newByteArray m
    fillByteArray marr 0 m 0x00
    go marr
    unsafeFreezeByteArray marr



{-# INLINE pointer0 #-}
pointer0 :: Feed -> RadixTree a -> Maybe Pointer
pointer0 (Feed feed) = \(RadixTree mx t) ->
  feed $ \step s ->
    case step s of
      More w z -> pointer_ step w z t
      Done     ->
        case mx of
          Just _  -> Just $ Pointer 0 emptyByteArray
          Nothing -> Nothing

{-# INLINE pointer1 #-}
pointer1 :: Feed1 -> Radix1Tree a -> Maybe Pointer
pointer1 (Feed1 w feed) = feed $ \step -> pointer_ step w

{-# INLINE pointer_ #-}
pointer_
  :: (x -> Step Word8 x)
  -> Word8 -> x -> Radix1Tree a -> Maybe Pointer
pointer_ (step :: x -> Step Word8 x) = go 0 (\_ -> pure ())
  where
    go :: Word -> (forall s. MutableByteArray s -> ST s ())
       -> Word8 -> x -> Radix1Tree a -> Maybe Pointer
    go !i acc !w !s t =
      case t of
        Bin p l r ->
          if w < p
            then go (i + 1)                          acc       w s l
            else go (i + 1) (\marr -> mark marr i >> acc marr) w s r

        Tip arr mx dx -> goarr w s 0
          where
            goarr v !z n
              | v == indexByteArray arr n =
                  let n' = n + 1
                  in if n' >= sizeofByteArray arr
                       then case step z of
                              More u z' -> go (i + 1) acc u z' dx
                              Done      ->
                                case mx of
                                  Just _  -> Just $ Pointer (i + 1) (form acc i)
                                  Nothing -> Nothing

                       else case step z of
                              More u z' -> goarr u z' n'
                              Done      -> Nothing

              | otherwise = Nothing

        Nil -> Nothing



follow0 :: a -> Pointer -> RadixTree a -> a
follow0 d (Pointer len arr) (RadixTree mx dx)
  | len == 0  = case mx of
                  Just x  -> x
                  Nothing -> d

  | otherwise = follow_ d len arr dx

follow1 :: a -> Pointer -> Radix1Tree a -> a
follow1 d (Pointer len arr) = follow_ d len arr

follow_ :: a -> Word -> ByteArray -> Radix1Tree a -> a
follow_ d len arr = go 0
  where
    go !i t =
      case t of
        Bin _ l r ->
          go (i + 1) $ if left arr i
                         then l
                         else r

        Tip _ mx dx ->
          let i' = i + 1
          in if i' > len
               then d
               else if i' == len
                      then case mx of
                             Just x  -> x
                             Nothing -> d

                      else go i' dx

        Nil -> d
