{-# LANGUAGE BangPatterns #-}

{-| Spine-strict radix tree location manipulation.

    Allows lookup and successive insertion without retaining the entirety
    of the key in memory.
 -}

module Data.Radix1Tree.Word8.Strict.Zipper
  ( Context1
  , descend
  , focus
  ) where

import           Data.ByteArray.NonEmpty
import           Data.RadixNTree.Word8.Key
import           Data.RadixNTree.Word8.Strict
import           Radix.Word8.Foundation

import           Data.Primitive.ByteArray



data Past a = Leftward
                !(Past a)
                {-# UNPACK #-} !Prefix
                !(Radix1Tree a)

            | Rightward
                !(Past a)
                {-# UNPACK #-} !Prefix
                !(Radix1Tree a)

            | Downward
                !(Past a)
                {-# UNPACK #-} !ByteArray
                {-# UNPACK #-} !(Maybe a)

            | Top


-- | A location inside the radix tree.
data Context1 a = -- | Corresponds to a 'Tip'.
                  Context1
                    !(Past a)
                    {-# UNPACK #-} !Int       -- ^ Next index in the byte array.
                    {-# UNPACK #-} !ByteArray
                    {-# UNPACK #-} !(Maybe a)
                    !(Radix1Tree a)



{-# INLINE descend #-}
-- | \(\mathcal{O}(\min(x,k))\).
--   Move down the tree by the extent of the given key.
--   Returns 'Nothing' if the resulting position is outside of the tree.
--
--   @since 1.1
descend :: Feed1 -> Either (Radix1Tree a) (Context1 a) -> Maybe (Context1 a)
descend (Feed1 w0 feed) =
  feed $ \step ->

    let go !past !w !s t =
          case t of
            Bin p l r ->
              if w < p
                then go (Leftward  past p r) w s l
                else go (Rightward past p l) w s r

            Tip arr mx dx -> goarr past arr mx dx w s 0

            Nil -> Nothing

        goarr !past !arr !mx dx = goarr_
          where
            goarr_ v !z n
              | v == indexByteArray arr n =
                  let n' = n + 1
                  in case step z of
                       More u z' -> if n' >= sizeofByteArray arr
                                      then go (Downward past arr mx) u z' dx
                                      else goarr_ u z' n'

                       Done      -> Just $! Context1 past n' arr mx dx

              | otherwise = Nothing

    in \s0 ei ->
         case ei of
           Left r                           -> go Top w0 s0 r
           Right (Context1 past n arr mx dx) ->
             if n == sizeofByteArray arr
               then go (Downward past arr mx) w0 s0 dx
               else goarr past arr mx dx w0 s0 n



-- | \(\mathcal{O}(1)\).
--   Retrieve the value at the current position, if any exists,
--   together with the insertion function for the current position.
--
--   @since 1.1
focus :: Context1 a -> Maybe (a, a -> Radix1Tree a)
focus (Context1 past n arr mx dx)
  | n == sizeofByteArray arr, Just x <- mx =
      Just $! (x, \y -> rebuild (Tip arr (Just y) dx) past)

  | otherwise = Nothing



rebuild :: Radix1Tree a -> Past a -> Radix1Tree a
rebuild !x past =
  case past of
    Leftward  past' p r   -> rebuild (Bin p x r) past'
    Rightward past' p l   -> rebuild (Bin p l x) past'
    Downward past' brr my -> rebuild (Tip brr my x) past'
    Top                   -> x
