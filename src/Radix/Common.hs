{-# LANGUAGE GADTs
           , UnboxedTuples #-}

module Radix.Common
  ( PartialOrdering (..)
  , order

  , S (..)
  , other
  , limit
  ) where



-- | Comparison of two sets, \(A\) and \(B\) respectively.
data PartialOrdering = Subset       -- ^ \(A \subset B\).
                     | Superset     -- ^ \(A \supset B\).
                     | Equal        -- ^ \(A = B\).
                     | Incomparable -- ^ \(A \parallel B\).
                       deriving (Show, Eq)

-- | Comparison of two partial orderings.
order :: PartialOrdering -> PartialOrdering -> PartialOrdering
order Subset   Subset   = Subset
order Subset   Equal    = Subset

order Superset Superset = Superset
order Superset Equal    = Superset

order Equal    o        = o

order _        _        = Incomparable



-- | Merge side.
data S a b x y where
  L :: S x y x y
  R :: S y x x y

-- | The other merge side.
other :: S a b x y -> (# S b a x y #)
other L = (# R #)
other R = (# L #)

-- | Limits the left side to a 'Subset'.
limit :: S x y a b -> PartialOrdering -> PartialOrdering
limit L Superset = Incomparable
limit R Subset   = Incomparable
limit s Equal    = case s of
                     L -> Subset
                     R -> Superset
limit _ o        = o
