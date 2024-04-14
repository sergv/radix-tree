{-# LANGUAGE BangPatterns #-}

{-|
    Safe functions for datatype introspection.
 -}

module Data.Zebra.Word.Debug
  ( -- * Show
    showsTree

    -- * Validate
  , Validity (..)
  , Reason (..)
  , validate
  ) where

import           Data.Zebra.Word.Internal
import           Numeric.Long
import           Radix.Word.Foundation
import           Radix.Word.Debug



-- | \(\mathcal{O}(n)\).
--   Shows the internal structure of the tree.
showsTree :: Zebra -> ShowS
showsTree = go 0
  where
    go i t =
      mappend (replicate i ' ') .
        case t of
          Bin p l r ->
            showString "Bin " . showPrefix p . showChar '\n'
                              . go (i + 2) l . showChar '\n'
                              . go (i + 2) r

          Bla k     -> goTip Black k
          Whi k     -> goTip White k

          Nil c     -> showString "Nil " . showChar (color c)

    goTip c k =
      showString "Tip " . showLongBin k . showString " => " . showChar (color c)

    color Black = 'B'
    color White = 'W'



-- | Whether the tree is well-formed.
data Validity = Valid
              | Invalid Reason
                deriving Show

-- | Reason for why the tree is considered malformed.
data Reason = -- | Prefix is @0@.
              ZeroPrefix
              -- | Prefix below diverges from the prefix above
            | PrefixBelow Prefix Prefix
              -- | Key diverges the prefix above
            | KeyBelow Prefix Key
              -- | Nil is in the tree.
            | FoundNil
              -- | Tip has a value of zero despite not being the root.
            | ZeroKey
              -- | Key has the same color as the key to the left of it.
            | NoSwitch Color Key
              deriving Show

data Carry = Carry Color
           | Break Reason

-- | \(\mathcal{O}(n)\).
--   Checks whether the tree is well-formed.
validate :: Zebra -> Validity
validate t0 =
  case go0 t0 of
    Carry _ -> Valid
    Break r -> Invalid r
  where
    go0 t =
      case t of
        Bin p l r
          | p == 0    -> Break ZeroPrefix
          | otherwise ->
              case go L p l Nothing of
                Carry cR -> go R p r (Just cR)
                err      -> err

        Bla _ -> Carry Black
        Whi _ -> Carry White

        Nil _ -> Break FoundNil

    go s q x cL =
      case x of
        Bin p l r
          | p == 0                 -> Break ZeroPrefix
          | not $ validBelow q s p -> Break $ PrefixBelow q p
          | otherwise              ->
              case go L p l cL of
                Carry cR -> go R p r (Just cR)
                err      -> err

        Bla k -> goTip s q k cL Black
        Whi k -> goTip s q k cL White

        Nil _ -> Break FoundNil

    goTip s q k cL c
      | k == 0                 = Break ZeroKey
      | not $ validBelow q s k = Break $ KeyBelow q k
      | Just x <- cL, x == c   = Break $ NoSwitch c k
      | otherwise              = Carry c
