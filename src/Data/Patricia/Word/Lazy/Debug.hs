{-|
    Safe functions for datatype introspection.
 -}

module Data.Patricia.Word.Lazy.Debug
  ( -- * Show
    showsTree

    -- * Validate
  , Validity (..)
  , Reason (..)
  , validate
  ) where

import           Data.Patricia.Word.Debug
import           Data.Patricia.Word.Lazy.Internal
import           Numeric.Long
import           Radix.Word.Debug



-- | \(\mathcal{O}(n)\).
--   Shows the internal structure of the tree.
showsTree :: (a -> ShowS) -> Patricia a -> ShowS
showsTree f = go 0
  where
    go i t =
      mappend (replicate i ' ') .
        case t of
          Bin p l r ->
            showString "Bin " . showPrefix p . showChar '\n'
                              . go (i + 2) l . showChar '\n'
                              . go (i + 2) r

          Tip k a   ->
            showString "Tip " . showLongHex k . showString " => " . f a

          Nil       -> showString "Nil"



-- | \(\mathcal{O}(n)\).
--   Checks whether the tree is well-formed.
validate :: Patricia a -> Validity
validate t =
  case t of
    Bin p l r
      | p == 0    -> Invalid ZeroPrefix
      | otherwise ->
          case go L p l of
            Valid -> go R p r
            err   -> err

    Tip _ _ -> Valid

    Nil -> Valid
  where
    go s q x =
      case x of
        Bin p l r
          | p == 0                 -> Invalid ZeroPrefix
          | not $ validBelow q s p -> Invalid $ PrefixBelow q p
          | otherwise              ->
              case go L p l of
                Valid -> go R p r
                err   -> err

        Tip k _
          | not $ validBelow q s k -> Invalid $ KeyBelow q k
          | otherwise              -> Valid

        Nil -> Invalid $ MalformedBin q
