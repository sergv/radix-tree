{-# LANGUAGE RankNTypes #-}

module Test.RadixNTree.Word8.Sample
  ( Sample
  , zero
  , one
  , tip
  , bin
  , tiny
  , small
  , medium
--, large

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

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Word
import           System.Random



data Trees = Trees (NonEmpty Tree)
           | End
             deriving Show

data Tree = Tree
              (NonEmpty Word8)
              Bool             -- ^ Whether this point is a separate key in the tree
              Trees
            deriving Show



genTrees
  :: RandomGen g
  => Int         -- ^ Maximum branches on each level
  -> Int         -- ^ Maximum number of segments
  -> Int         -- ^ Maximum segment length
  -> Int         -- ^ Maximum total length
  -> g
  -> (Trees, g)
genTrees nB nL nS nT = broad nL nT
  where
    broad count len g0
      | len <= 0 || count <= 0 = (End, g0)
      | otherwise              =
          let ~(n, g1) = uniformR (1, nB) g0

              ~(as, g2) = list1 (deep count len) n g1

          in (Trees $ dedup as, g2)

    dedup = NonEmpty.nubBy (\(Tree (x :| _) _ _) (Tree (y :| _) _ _) -> x == y)

    deep count len g0 =
      let ~(n, g1) = uniformR (1, max 1 (min len nS)) g0

          ~(xs, g2) = list1 uniform n g1

          ~(t, g3) = broad (count - 1) (len - n) g2

          ~(bias, g4) = case t of
                          End -> (nL, g3)
                          _   -> uniformR (1, nL) g3

      in (Tree xs (bias == 1) t, g4)



timber :: Trees -> [([Word8], Int)]
timber = fst . broad id ([], 1)
  where
    broad pre ~(acc, n) End        = ((pre [], n) : acc, n + 1)
    broad pre z         (Trees ts) = foldr (flip $ deep pre) z ts

    deep pre z@(acc, n) (Tree xs real t) =
      let z' = if real
                 then ((pre $ NonEmpty.toList xs, n) : acc, n + 1)
                 else z

      in broad (pre . (NonEmpty.toList xs <>)) z' t



data Sample = Sample
                [(No.Openness, [Word8], Int)] -- ^ Keys in the dictionary
                [(No.Openness, [Word8], Int)] -- ^ Keys not in the dictionary
              deriving Show

halve :: [a] -> ([a], [a])
halve (a:b:cs) = let ~(xs, ys) = halve cs
                 in (a:xs, b:ys)
halve a        = (a, [])

sample :: RandomGen g => Trees -> g -> Sample
sample t g0 =
  let ~(xs, g1) = shuffle (timber t) g0

      ~(os, g2) = list (\g' -> let ~(b, g'') = uniform g'
                               in ( if b then No.Open else No.Closed
                                  , g''
                                  )
                       )
                       (length xs) g1

      xs' = zipWith (\a (b, c) -> (a, b, c)) os xs

      ~(ys, zs) = halve xs'

      ~(z, _) = uniform g2

      as | z         = case ys of
                         []               -> []
                         (b, _, i) : rest -> (b, [], i) : rest
         | otherwise = ys

      bs = case zs of
             []               -> []
             (b, _, i) : rest -> (b, [], i) : rest

  in Sample as bs



zero, one, tip, bin :: Sample
zero = Sample []
         [ (No.Open, [], 1), (No.Closed, [1, 2, 3], 2), (No.Open, [3, 2, 1], 3) ]

one  = Sample [(No.Open, [1, 2, 3], 0)]
         [ (No.Closed, [1, 2, 3], 1), (No.Open, [1, 2, 2], 2), (No.Closed, [1, 2, 4], 3)
         , (No.Open, [1, 2], 4), (No.Closed, [1, 2, 3, 4], 5), (No.Open, [2, 3, 4], 6)
         , (No.Closed, [], 7), (No.Open, [2], 8)
         ]

tip  = Sample [(No.Open, [], 0)]
         [ (No.Closed, [1, 2, 3], 1), (No.Closed, [], 2) ]

bin  = Sample [(No.Open, [1, 2, 2, 3], 0), (No.Closed, [1, 2, 4, 5], 1)]
         [ (No.Closed, [1, 2, 3, 4], 2), (No.Open, [1, 2, 2, 3], 3)
         , (No.Closed, [1, 2, 4, 5], 4), (No.Closed, [], 5)
         ]



tiny, small, medium :: Sample
tiny   = uncurry sample $ genTrees 4 2 4 16 (mkStdGen 2)
small  = uncurry sample $ genTrees 4 4 4 16 (mkStdGen 4)
medium = uncurry sample $ genTrees 8 4 4 16 (mkStdGen 16)



type FromList pat = forall x. [([Word8], x)] -> pat x

mkUnary0 :: FromList pat -> Sample -> [Case () (pat Int) (NoTree [Word8] Int)]
mkUnary0 patFromList (Sample xs _) =
  let as = fmap (\(_, k, i) -> (k, i)) xs

  in [Case () (patFromList as) (No.fromList as)]

mkUnary1
  :: FromList pat
  -> Sample -> [Case (No.Openness, [Word8], Int) (pat Int) (NoTree [Word8] Int)]
mkUnary1 patFromList (Sample xs ys) =
  let as = fmap (\(_, k, i) -> (k, i)) xs

      pat = patFromList as
      no  = No.fromList as

  in foldr (\x -> (:) (Case x pat no)) [] $ xs <> ys

mkUnary2
  :: FromList pat
  -> Sample
  -> [Case (No.Openness, [Word8], No.Openness, [Word8], Int) (pat Int) (NoTree [Word8] Int)]
mkUnary2 patFromList (Sample xs ys) =
  let xs' = fmap (\(_, k, i) -> (k, i)) xs

      pat = patFromList xs'
      no  = No.fromList xs'

      ~(as, bs) = halve xs
      ~(cs, ds) = halve ys

      ones = fmap (\(o, a, i) -> (o, a, o, a, i)) $ as <> cs

      twos = zipWith (\(o, a, i) (p, b, _) -> (o, a, p, b, i)) bs ds

  in foldr (\x -> (:) (Case x pat no)) [] $ ones <> twos



mkBinary
  :: FromList pat
  -> Sample
  -> [Case (pat Int, NoTree [Word8] Int) (pat Int) (NoTree [Word8] Int)]
mkBinary patFromList (Sample xs ys) =
  let as = fmap (\(_, k, i) -> (k, i)) xs
      bs = fmap (\(_, k, i) -> (k, i)) ys

  in [Case (patFromList bs, No.fromList bs) (patFromList as) (No.fromList as)]

mkBinaryL
  :: FromList pat
  -> Sample
  -> [Case (pat Int, NoTree [Word8] Int) (pat Int) (NoTree [Word8] Int)]
mkBinaryL patFromList (Sample xs ys) =
  let xs' = fmap (\(_, k, i) -> (k, i)) xs
      ys' = fmap (\(_, k, i) -> (k, i)) ys

      ~(as, _) = halve xs'
      ~(bs, _) = halve ys'

      ls = fmap (\(k, a) -> (k, negate a)) bs <> xs'
      rs = fmap (\(k, a) -> (k, negate a)) as <> ys'

  in [Case (patFromList rs, No.fromList rs) (patFromList ls) (No.fromList ls)]

mkEqual
  :: FromList pat
  -> Sample
  -> [Case (pat Int, NoTree [Word8] Int) (pat Int) (NoTree [Word8] Int)]
mkEqual patFromList (Sample xs _) =
  let as = fmap (\(_, k, i) -> (k, i)) xs

      pat = patFromList as
      no  = No.fromList as

  in [Case (pat, no) pat no]

mkSuperset
  :: FromList pat
  -> Sample
  -> [Case (pat Int, NoTree [Word8] Int) (pat Int) (NoTree [Word8] Int)]
mkSuperset patFromList (Sample xs ys) =
  let as = fmap (\(_, k, i) -> (k, i)) xs
      bs = fmap (\(_, k, i) -> (k, i)) ys

      zs = as <> bs

  in [Case (patFromList zs, No.fromList zs) (patFromList as) (No.fromList as)]

mkSubset
  :: FromList pat
  -> Sample
  -> [Case (pat Int, NoTree [Word8] Int) (pat Int) (NoTree [Word8] Int)]
mkSubset patFromList (Sample xs ys) =
  let as = fmap (\(_, k, i) -> (k, i)) xs
      bs = fmap (\(_, k, i) -> (k, i)) ys

      zs = as <> bs

  in [Case (patFromList as, No.fromList as) (patFromList zs) (No.fromList zs)]
