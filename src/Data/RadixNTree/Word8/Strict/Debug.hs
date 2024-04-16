module Data.RadixNTree.Word8.Strict.Debug
  ( showsTree0
  , showsTree1

  , Validity (..)
  , Reason (..)
  , validate0
  , validate1
  ) where

import           Data.ByteArray.NonEmpty
import           Data.RadixNTree.Word8.Debug
import           Data.RadixNTree.Word8.Key
import           Data.RadixNTree.Word8.Strict
import           Numeric.Long
import           Radix.Word8.Debug

import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Primitive.ByteArray



showsTree0 :: (a -> ShowS) -> RadixTree a -> ShowS
showsTree0 f (RadixTree mx t) =
  showString "RadixTree" . case mx of
                             Just x  -> showString " => " . f x
                             Nothing -> id

                         . showChar '\n'

                         . showsTree_ 2 f t

showsTree1 :: (a -> ShowS) -> Radix1Tree a -> ShowS
showsTree1 f = showsTree_ 0 f

showsTree_ :: Int -> (a -> ShowS) -> Radix1Tree a -> ShowS
showsTree_ n0 f = go n0
  where
    go i t =
      mappend (replicate i ' ') .
        case t of
          Bin p l r   ->
            showString "Bin " . showPrefix p . showChar '\n'
                              . go (i + 2) l . showChar '\n'
                              . go (i + 2) r

          Tip arr mx dx ->
            showString "Tip " . if sizeofByteArray arr <= 0
                                  then id
                                  else let w0 :| ws = toNonEmpty arr
                                       in showLongBin w0
                                            . showString " (" . showLongHex w0 . showChar ')'
                                            . foldr (\x s -> showChar ' ' . showLongHex x . s) id ws

                                 . case mx of
                                     Just x  -> showString " => " . f x
                                     Nothing -> id

                                 . showChar '\n'

                                 . go (i + 2) dx

          Nil           -> showString "Nil"



validate0 :: RadixTree a -> Validity
validate0 (RadixTree _ t) = validate1 t

validate1 :: Radix1Tree a -> Validity
validate1 = go Lin
  where
    go b t =
      case t of
        Bin p l r
          | p == 0                 -> Invalid (Build b) ZeroPrefix
          | otherwise              ->
              case goBin L b p l of
                Valid -> goBin R b p r
                err   -> err

        Tip arr mx dx
          | sizeofByteArray arr <= 0       -> Invalid (Build b) EmptyByteArray
          | Nothing <- mx, Tip _ _ _ <- dx -> Invalid (Build b) UncompressedTip
          | Nothing <- mx, Nil       <- dx -> Invalid (Build b) UncompressedTip
          | otherwise                      -> go (Snoc b arr) dx

        Nil -> Valid

    goBin s b q x =
      case x of
        Bin p l r
          | p == 0                 -> Invalid (Build b) ZeroPrefix
          | not $ validBelow q s p -> Invalid (Build b) $ PrefixBelow q p
          | otherwise              ->
              case goBin L b p l of
                Valid -> goBin R b p r
                err   -> err

        Tip arr mx dx
          | sizeofByteArray arr <= 0                    -> Invalid (Build b) EmptyByteArray
          | not $ validBelow q s (indexByteArray arr 0) ->
              Invalid (Build b) $ KeyBelow q (indexByteArray arr 0)

          | Nothing <- mx, Tip _ _ _ <- dx     -> Invalid (Build b) UncompressedTip
          | Nothing <- mx, Nil       <- dx     -> Invalid (Build b) UncompressedTip
          | otherwise                          -> go (Snoc b arr) dx

        Nil -> Invalid (Build b) $ MalformedBin q
