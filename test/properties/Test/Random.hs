module Test.Random
  ( list
  , list1
  , shuffle
  ) where

import           Data.List
import           Data.List.NonEmpty (NonEmpty (..))
import           System.Random



list :: (g -> (a, g)) -> Int -> g -> ([a], g)
list gen = go
  where
    go n g
      | n <= 0    = ([], g)
      | otherwise = let ~(a, g')   = gen g
                        ~(as, g'') = go (n - 1) g'
                    in (a:as, g'')

list1 :: (g -> (a, g)) -> Int -> g -> (NonEmpty a, g)
list1 gen n g =
  let ~(a, g') = gen g
  in if n <= 1
       then (a :| [], g')
       else let ~(as, g'') = list gen (n - 1) g'
            in (a :| as, g'')



shuffle :: RandomGen g => [a] -> g -> ([a], g)
shuffle as g = let ~(bs, g') = ziplist as g
               in (fmap snd $ sortBy (\a b -> fst a `compare` fst b) bs, g')
  where
    ziplist :: RandomGen g => [a] -> g -> ([(Int, a)], g)
    ziplist xs h =
      case xs of
        []   -> ([], h)
        x:ys -> let ~(n, h')   = uniform h
                    ~(zs, h'') = ziplist ys h'
                in ((n, x):zs, h'')
