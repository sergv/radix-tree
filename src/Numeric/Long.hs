{-# LANGUAGE ScopedTypeVariables #-}

module Numeric.Long where

import           Data.Bits
import           Data.Char
import           Numeric (showHex)



showLongHex :: (FiniteBits a, Integral a, Num a) => a -> ShowS
showLongHex (w0 :: a) = go w0 0
  where
    go w n
      | n >= finiteBitSize (0 :: a) = id
      | otherwise                   =
          let (q, r) = quotRem w 16
          in go q (n + 4 :: Int) . showHex r



showLongHexCutoff :: (FiniteBits a, Integral a, Num a) => Int -> a -> ShowS
showLongHexCutoff c (w0 :: a) = go w0 0
  where
    go w n
      | n >= finiteBitSize (0 :: a) = id
      | otherwise                   =
          let (q, r) = quotRem w 16
          in go q (n + 4 :: Int) .
               if n + 3 < c
                 then showChar '_'
                 else showHex r



showLongBin :: (FiniteBits a, Integral a, Num a) => a -> ShowS
showLongBin (w :: a) = go 0
  where
    go n
      | n >= finiteBitSize (0 :: a) = id
      | otherwise                   =
          go (n + 1) . showChar (chr . fromIntegral $ 48 + (unsafeShiftR w n .&. 1))



showPrefix :: (FiniteBits a, Integral a, Num a) => a -> ShowS
showPrefix (w :: a) = go 0
  where
    m = w .&. negate w

    go n
      | n >= finiteBitSize (0 :: a) = id
      | otherwise                   =
          go (n + 1) . showChar
                         ( if unsafeShiftL 1 n >= m
                             then chr . fromIntegral $ 48 + (unsafeShiftR w n .&. 1)
                             else 'X'
                         )




showLongBinCutoff :: (FiniteBits a, Integral a, Num a) => Int -> a -> ShowS
showLongBinCutoff c (w0 :: a) = go w0 0
  where
    go w n
      | n >= finiteBitSize (0 :: a) = id
      | otherwise                   =
          let (q, r) = quotRem w 2
          in go q (n + 1 :: Int) .
               case (n + 1) `compare` c of
                 LT -> showChar '_'
                 EQ -> showChar 'X'
                 GT -> showHex r
