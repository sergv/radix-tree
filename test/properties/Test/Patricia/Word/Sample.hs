{-# LANGUAGE RankNTypes #-}

module Test.Patricia.Word.Sample
  ( Sample
  , zero
  , one
  , tiny
  , small
  , medium
  , large

  , mkUnary0
  , mkUnary1
  , mkUnary2

  , mkBinary
  , mkBinaryL

  , mkEqual
  , mkSuperset
  , mkSubset
  ) where

import           No.Tree (NoTree)
import qualified No.Tree as No
import           Test.Kit
import           Test.Random

import qualified Data.List as List
import           System.Random



data Sample = Sample
                [(Word, Int)] -- ^ Keys in the dictionary
                [(Word, Int)] -- ^ Keys not in the dictionary
              deriving Show

zero, one :: Sample
zero = Sample [] [(4507, 1), (5824, 2), (6183, 3), (6858, 4)]
one  = Sample [(6593, 0)]
              [(4905, 1), (6285, 2), (6134, 3), (6737, 4), (6928, 5), (7513, 6)]



halve :: [a] -> ([a], [a])
halve (a:b:cs) = let ~(xs, ys) = halve cs
                 in (a:xs, b:ys)
halve a        = (a, [])

sample :: (Word, Word) -> Int -> StdGen -> Sample
sample r n g =
  let ~(xs, _) = list (uniformR r) n g

      ~(ys, zs) = halve $ zip (List.nub xs) [0..]

  in Sample ys zs

tiny, small, medium, large :: Sample
tiny   = sample (0x1000, 0x80000) 8    (mkStdGen 0)
small  = sample (0x1000, 0x80000) 64   (mkStdGen 1)
medium = sample (0x1000, 0x80000) 512  (mkStdGen 2)
large  = sample (0x1000, 0x80000) 4096 (mkStdGen 3)



type FromList pat = forall x. [(Word, x)] -> pat x

mkUnary0 :: FromList pat -> Sample -> [Case () (pat Int) (NoTree Word Int)]
mkUnary0 patFromList (Sample xs _) = [Case () (patFromList xs) (No.fromList xs)]

mkUnary1 :: FromList pat -> Sample -> [Case (Word, Int) (pat Int) (NoTree Word Int)]
mkUnary1 patFromList (Sample xs ys) =
  let pat = patFromList xs
      no  = No.fromList xs

  in foldr (\x -> (:) (Case x pat no)) [] $ xs <> ys

mkUnary2
  :: FromList pat -> Sample -> [Case (Word, Word, Int) (pat Int) (NoTree Word Int)]
mkUnary2 patFromList (Sample xs ys) =
  let pat = patFromList xs
      no  = No.fromList xs

      ~(as, bs) = halve xs
      ~(cs, ds) = halve ys

      ones = fmap (\(a, i) -> (a, a, i)) $ as <> cs

      twos = zipWith (\(a, i) (b, _) -> (a, b, i)) bs ds

  in foldr (\x -> (:) (Case x pat no)) [] $ ones <> twos


mkBinary
  :: FromList pat
  -> Sample
  -> [Case (pat Int, NoTree Word Int) (pat Int) (NoTree Word Int)]
mkBinary patFromList (Sample xs ys) =
  [Case (patFromList ys, No.fromList ys) (patFromList xs) (No.fromList xs)]

mkBinaryL
  :: FromList pat
  -> Sample
  -> [Case (pat Int, NoTree Word Int) (pat Int) (NoTree Word Int)]
mkBinaryL patFromList (Sample xs ys) =
  let ~(as, _) = halve xs
      ~(bs, _) = halve ys

      ls = fmap (\(k, a) -> (k, negate a)) bs <> xs
      rs = fmap (\(k, a) -> (k, negate a)) as <> ys

  in [Case (patFromList rs, No.fromList rs) (patFromList ls) (No.fromList ls)]


mkEqual
  :: FromList pat
  -> Sample
  -> [Case (pat Int, NoTree Word Int) (pat Int) (NoTree Word Int)]
mkEqual patFromList (Sample xs _) =
  let pat = patFromList xs
      no  = No.fromList xs

  in [Case (pat, no) pat no]

mkSuperset
  :: FromList pat
  -> Sample
  -> [Case (pat Int, NoTree Word Int) (pat Int) (NoTree Word Int)]
mkSuperset patFromList (Sample xs ys) =
  let zs = xs <> ys
  in [Case (patFromList zs, No.fromList zs) (patFromList xs) (No.fromList xs)]

mkSubset
  :: FromList pat
  -> Sample
  -> [Case (pat Int, NoTree Word Int) (pat Int) (NoTree Word Int)]
mkSubset patFromList (Sample xs ys) =
  let zs = xs <> ys
  in [Case (patFromList xs, No.fromList xs) (patFromList zs) (No.fromList zs)]
