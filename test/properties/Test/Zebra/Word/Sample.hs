{-# LANGUAGE RankNTypes #-}

module Test.Zebra.Word.Sample
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

  , mkBinaryL
  , mkEqual
  , mkSubset

  , tipsA
  , mkTipTip
  , tipsB
  , mkTipBin
  ) where

import           No.Set.Word (NoSet)
import qualified No.Set.Word as No
import           Test.Kit
import           Test.Random

import           Data.Foldable (foldl')
import           Data.Function
import qualified Data.List as List
import           System.Random



data Sample = Sample
                No.Color           -- ^ Color of negative infinity in the set
                [(Word, No.Color)] -- ^ Keys in the set (colors are arbitrary)
                [(Word, No.Color)] -- ^ Keys not in the set (colors are arbitrary)
              deriving Show

zero, one :: Sample
zero = Sample No.Black []
         [ (0, No.Black), (5824, No.White), (6183, No.Black), (maxBound, No.White)
         ]

one  = Sample No.White [(6593, No.Black)]
         [ (0   , No.Black), (4905, No.White), (6285, No.Black), (6134    , No.White)
         , (6737, No.Black), (6928, No.White), (7513, No.Black), (maxBound, No.White)
         ]



halve :: [a] -> ([a], [a])
halve (a:b:cs) = let ~(xs, ys) = halve cs
                 in (a:xs, b:ys)
halve a        = (a, [])

color :: Bool -> No.Color
color False = No.Black
color True  = No.White

sample :: RandomGen g => (Word, Word) -> Int -> g -> (Sample, g)
sample r n g0 =
  let ~(c0, g1) = uniform g0

      ~(xs, g2) = list (\g' -> let ~(w, g'') = uniformR r g'
                                   ~(c, _)   = uniform g''

                               in ((w, color c), g'')
                       )
                    n g1

      cs = List.nub $ List.sortBy (compare `on` fst) xs

      ~(as, bs) = halve cs

  in (Sample (color c0) as bs, g2)



-- | Function that fills the space in the \((+\infty, k]\) range with the given color.
type FillL set = Word -> No.Color -> set -> set

type FromList set = No.Color
                    -- ^ Color of positive infinity

                 -> (FillL set -> set -> set)
                    -- ^ Application of every other color.

                 -> set

foldrFromList :: FromList set -> No.Color -> [(Word, No.Color)] -> set
foldrFromList f c xs = f c (\g s0 -> List.foldr (uncurry g) s0 xs)

noFromList :: FromList NoSet
noFromList c f = f No.fillL (No.Mono c)

setFromNo :: Show set => FromList set -> NoSet -> set
setFromNo setFromList no =
  case No.foldl (\z r c -> (r, c) : z) [] no of
    []          -> error "Zebra.Sample: empty NoSet"
    (_, c) : ys -> setFromList c $ \f s -> foldl' (\z (No.Range _ b, x) -> f b x z) s ys

tiny, small, medium, large :: Sample
tiny   = fst $ sample (0x1000, 0x80000) 8    (mkStdGen 0)
small  = fst $ sample (0x1000, 0x80000) 64   (mkStdGen 1)
medium = fst $ sample (0x1000, 0x80000) 512  (mkStdGen 2)
large  = fst $ sample (0x1000, 0x80000) 4096 (mkStdGen 3)



mkUnary0 :: FromList set -> Sample -> [Case () set NoSet]
mkUnary0 setFromList (Sample c xs _) =
  [Case () (foldrFromList setFromList c xs) (foldrFromList noFromList c xs)]

mkUnary1 :: FromList set -> Sample -> [Case (Word, No.Color) set NoSet]
mkUnary1 setFromList (Sample c xs ys) =
  let set = foldrFromList setFromList c xs
      no  = foldrFromList noFromList c xs

  in foldr (\x -> (:) (Case x set no)) [] $
       (:) (0, No.Black) . (:) (maxBound, No.White) $ xs <> ys

mkUnary2 :: FromList set -> Sample -> [Case (No.Range, No.Color) set NoSet]
mkUnary2 setFromList (Sample c xs ys) =
  let set = foldrFromList setFromList c xs
      no  = foldrFromList noFromList c xs

      ~(as, bs) = halve xs
      ~(cs, ds) = halve ys

      ones = fmap (\(a, i) -> (No.UnsafeRange a a, i)) $
               (:) (0, No.White) . (:) (maxBound, No.Black) $ as <> cs

      es = List.nub . List.sortBy (compare `on` fst) $ bs <> ds

      twos = (:) (No.UnsafeRange 0       0x65432 , No.Black)
           . (:) (No.UnsafeRange 0x54321 maxBound, No.White)
           . (:) (No.UnsafeRange 0       maxBound, No.White)
           $ unsafeRanges es

  in foldr (\x -> (:) (Case x set no)) [] $ ones <> twos
  where
    -- | Converts an ascending list of integers into a list of ranges.
    unsafeRanges :: [(Word, No.Color)] -> [(No.Range, No.Color)]
    unsafeRanges ((a, x):(b, _):cs) = (No.UnsafeRange a b, x) : unsafeRanges cs
    unsafeRanges _                  = []



mkBinaryL :: FromList set -> Sample -> [Case (set, NoSet) set NoSet]
mkBinaryL setFromList (Sample c xs ys) =
  let set1 = foldrFromList setFromList c xs
      no1  = foldrFromList noFromList c xs

      set2 = foldrFromList setFromList c ys
      no2  = foldrFromList noFromList c ys

  in [Case (set2, no2) set1 no1]


mkEqual :: FromList set -> Sample -> [Case (set, NoSet) set NoSet]
mkEqual setFromList (Sample c xs _) =
  let set = foldrFromList setFromList c xs
      no  = foldrFromList noFromList c xs

  in [Case (set, no) set no]

mkSubset :: Show set => FromList set -> No.Color -> Sample -> [Case (set, NoSet) set NoSet]
mkSubset setFromList x (Sample c xs ys) =
  let set = foldrFromList setFromList c xs

      no  = foldrFromList noFromList c xs
      no' = foldrFromList noFromList c ys

      noI = No.intersection x no no'

  in [Case (setFromNo setFromList noI, noI) set no]



tipA :: RandomGen g => g -> ((No.Color, Word, No.Color, Word), g)
tipA g0 =
  let ~(c1, g1) = uniform g0
      ~(w1, g2) = uniform g1

      ~(c2, g3) = uniform g2
      ~(w2, g4) = uniform g3

  in ((if c1 then No.White else No.Black, w1, if c2 then No.White else No.Black, w2), g4)

tipsA :: [(No.Color, Word, No.Color, Word)]
tipsA = fst $ list tipA 10000 (mkStdGen 0)

mkTipTip :: FromList set -> No.Color -> Word -> No.Color -> Word -> [Case (set, NoSet) set NoSet]
mkTipTip setFromList c1 w1 c2 w2 =
  let set1 = foldrFromList setFromList c1 [(w1, No.other c1)]
      no1  = foldrFromList noFromList c1 [(w1, No.other c1)]

      set2 = foldrFromList setFromList c2 [(w2, No.other c2)]
      no2  = foldrFromList noFromList c2 [(w2, No.other c2)]

  in [Case (set2, no2) set1 no1]



tipB :: RandomGen g => g -> ((No.Color, Word, Sample), g)
tipB g0 =
  let ~(c1, g1) = uniform g0
      ~(w1, g2) = uniform g1

      ~(s, g3) = sample (0, maxBound) 16 g2

  in ((if c1 then No.White else No.Black, w1, s), g3)

tipsB :: [(No.Color, Word, Sample)]
tipsB = fst $ list tipB 1000 (mkStdGen 0)

mkTipBin :: FromList set -> No.Color -> Word -> Sample -> [Case (set, NoSet) set NoSet]
mkTipBin setFromList c1 w1 (Sample c2 xs ys) =
  let set1 = foldrFromList setFromList c1 [(w1, No.other c1)]
      no1  = foldrFromList noFromList c1 [(w1, No.other c1)]

      set2 = foldrFromList setFromList c2 xs
      no2  = foldrFromList noFromList c2 xs

      (setA, noA, setB, noB) | (_, No.Black):_ <- ys = (set2, no2, set1, no1)
                             | otherwise             = (set1, no1, set2, no2)

  in [Case (setB, noB) setA noA]
