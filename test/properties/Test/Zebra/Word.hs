{-# LANGUAGE RankNTypes #-}

module Test.Zebra.Word
  ( test
  ) where

import           Data.Zebra.Word (Zebra, Color (..), Range (..))
import qualified Data.Zebra.Word as Zebra
import           Data.Zebra.Word.Debug
import           No.Set.Word (NoSet)
import qualified No.Set.Word as No
import           Test.Kit
import           Test.Zebra.Word.Sample

import           Numeric.Natural
import           Test.Hspec



setFromList :: Color -> ((Word -> Color -> Zebra -> Zebra) -> Zebra -> a) -> a
setFromList c f = f Zebra.fillL (Zebra.Mono c)

setToList :: Zebra -> [(Color, Word, Word)]
setToList = Zebra.foldr (\(Range kL kR) c -> (:) (c, kL, kR)) []

noToList :: NoSet -> [(Color, Word, Word)]
noToList = No.foldr (\(Range kL kR) c -> (:) (c, kL, kR)) []



unary0 :: [Case () Zebra NoSet]
unary0 = foldMap (mkUnary0 setFromList) [zero, one, tiny, small, medium] --, large]



unary1 :: [Case (Word, Color) Zebra NoSet]
unary1 = foldMap (mkUnary1 setFromList) [zero, one, tiny, small, medium] --, large]

unary1_ :: [Case Word Zebra NoSet]
unary1_ = augment fst unary1


unary2 :: [Case (Range, Color) Zebra NoSet]
unary2 = foldMap (mkUnary2 setFromList) [zero, one, tiny, small, medium] --, large]

unary2_ :: [Case Range Zebra NoSet]
unary2_ = augment fst unary2



binaryL, equal :: [Case (Zebra, NoSet) Zebra NoSet]
binaryL  = foldMap (mkBinaryL  setFromList) [zero, one, tiny, small, medium] --, large]
equal    = foldMap (mkEqual    setFromList) [zero, one, tiny, small, medium] --, large]

subset :: Color -> [Case (Zebra, NoSet) Zebra NoSet]
subset c = foldMap (mkSubset setFromList c) [zero, one, tiny, small, medium] --, large]



-- Tip/Tip combinations.
_tipTip :: [Case (Zebra, NoSet) Zebra NoSet]
_tipTip = foldMap (\(a, b, c, d) -> mkTipTip setFromList a b c d) tipsA

-- Tip/Bin combinations.
_tipBin :: [Case (Zebra, NoSet) Zebra NoSet]
_tipBin = foldMap (\(a, b, s) -> mkTipBin setFromList a b s) tipsB



type IdT s b = Test s Zebra NoSet b b

type TreeT s = Test s Zebra NoSet Zebra NoSet

treeEq :: Zebra -> NoSet -> Bool
treeEq set no =
  case validate set of
    Valid -> setToList set == noToList no
    _     -> False



lookupT :: IdT Word Color
lookupT = Test (==) Zebra.lookup No.lookup

lookupLT :: IdT (Word, Color) (Maybe Word)
lookupLT = Test (==) (\(k, c) -> Zebra.lookupL c k)
                     (\(k, c) -> No.lookupL c k)

findLT :: IdT (Word, Color) Word
findLT = Test (==) (\(k, c) -> Zebra.findL (maxBound - 5) c k)
                   (\(k, c) -> No.findL (maxBound - 5) c k)

lookupRT :: IdT (Word, Color) (Maybe Word)
lookupRT = Test (==) (\(k, c) -> Zebra.lookupR c k)
                     (\(k, c) -> No.lookupR c k)

findRT :: IdT (Word, Color) Word
findRT = Test (==) (\(k, c) -> Zebra.findR (maxBound - 5) c k)
                   (\(k, c) -> No.findR (maxBound - 5) c k)



monoT :: IdT () (Maybe Color)
monoT = Test (==) ( \_ t -> case t of
                              Zebra.Mono c -> Just c
                              _            -> Nothing
                  )
                  ( \_ t -> case t of
                              No.Mono c -> Just c
                              _         -> Nothing
                  )

monoLT :: IdT Word (Maybe Color)
monoLT = Test (==) Zebra.monoL No.monoL

monoRT :: IdT Word (Maybe Color)
monoRT = Test (==) Zebra.monoR No.monoR

monoRangeT :: IdT Range (Maybe Color)
monoRangeT = Test (==) Zebra.monoRange No.monoRange



sizeT :: No.Color -> IdT () Natural
sizeT c = Test (==) (\_ -> Zebra.size c) (\_ -> No.size c)

sizeLT :: IdT (Word, Color) Natural
sizeLT = Test (==) (uncurry $ flip Zebra.sizeL) (uncurry $ flip No.sizeL)

sizeRT :: IdT (Word, Color) Natural
sizeRT = Test (==) (uncurry $ flip Zebra.sizeR) (uncurry $ flip No.sizeR)

sizeRangeT :: IdT (Range, Color) Natural
sizeRangeT = Test (==) (uncurry $ flip Zebra.sizeRange) (uncurry $ flip No.sizeRange)



fillLT :: TreeT (Word, Color)
fillLT = Test treeEq (uncurry Zebra.fillL) (uncurry No.fillL)

fillRT :: TreeT (Word, Color)
fillRT = Test treeEq (uncurry Zebra.fillR) (uncurry No.fillR)

fillRangeT :: TreeT (Range, Color)
fillRangeT = Test treeEq (uncurry Zebra.fillRange) (uncurry No.fillRange)



complementT :: TreeT ()
complementT = Test treeEq (\_ -> Zebra.complement) (\_ -> No.complement)



foldlT, foldlT' :: IdT () [(Color, Word, Word)]
foldlT  = foldlT_ Zebra.foldl
foldlT' = foldlT_ Zebra.foldl'

foldlT_
  :: (forall x. (x -> Range -> Color -> x) -> x -> Zebra -> x)
  -> IdT () [(Color, Word, Word)]
foldlT_ g =
  let f z (Range kL kR) c = (c, kL, kR) : z
  in Test (==) (\_ -> g f []) (\_ -> No.foldl f [])


foldlLT, foldlLT' :: IdT Word [(Color, Word, Word)]
foldlLT  = foldlLT_ Zebra.foldlL
foldlLT' = foldlLT_ Zebra.foldlL'

foldlLT_
  :: (forall x. Word -> (x -> Range -> Color -> x) -> x -> Zebra -> x)
  -> IdT Word [(Color, Word, Word)]
foldlLT_ g =
  let f z (Range kL kR) c = (c, kL, kR) : z
  in Test (==) (\w -> g w f []) (\w -> No.foldlL w f [])


foldlRT, foldlRT' :: IdT Word [(Color, Word, Word)]
foldlRT  = foldlRT_ Zebra.foldlR
foldlRT' = foldlRT_ Zebra.foldlR'

foldlRT_
  :: (forall x. Word -> (x -> Range -> Color -> x) -> x -> Zebra -> x)
  -> IdT Word [(Color, Word, Word)]
foldlRT_ g =
  let f z (Range kL kR) c = (c, kL, kR) : z
  in Test (==) (\w -> g w f []) (\w -> No.foldlR w f [])


foldlRangeT, foldlRangeT' :: IdT Range [(Color, Word, Word)]
foldlRangeT  = foldlRangeT_ Zebra.foldlRange
foldlRangeT' = foldlRangeT_ Zebra.foldlRange'

foldlRangeT_
  :: (forall x. Range -> (x -> Range -> Color -> x) -> x -> Zebra -> x)
  -> IdT Range [(Color, Word, Word)]
foldlRangeT_ g =
  let f z (Range kL kR) c = (c, kL, kR) : z
  in Test (==) (\w -> g w f []) (\w -> No.foldlRange w f [])



foldrT, foldrT' :: IdT () [(Color, Word, Word)]
foldrT  = foldrT_ Zebra.foldr
foldrT' = foldrT_ Zebra.foldr'

foldrT_
  :: (forall x. (Range -> Color -> x -> x) -> x -> Zebra -> x)
  -> IdT () [(Color, Word, Word)]
foldrT_ g =
  let f (Range kL kR) c = (:) (c, kL, kR)
  in Test (==) (\_ -> g f []) (\_ -> No.foldr f [])


foldrLT, foldrLT' :: IdT Word [(Color, Word, Word)]
foldrLT  = foldrLT_ Zebra.foldrL
foldrLT' = foldrLT_ Zebra.foldrL'

foldrLT_
  :: (forall x. Word -> (Range -> Color -> x -> x) -> x -> Zebra -> x)
  -> IdT Word [(Color, Word, Word)]
foldrLT_ g =
  let f (Range kL kR) c = (:) (c, kL, kR)
  in Test (==) (\w -> g w f []) (\w -> No.foldrL w f [])


foldrRT, foldrRT' :: IdT Word [(Color, Word, Word)]
foldrRT  = foldrRT_ Zebra.foldrR
foldrRT' = foldrRT_ Zebra.foldrR'

foldrRT_
  :: (forall x. Word -> (Range -> Color -> x -> x) -> x -> Zebra -> x)
  -> IdT Word [(Color, Word, Word)]
foldrRT_ g =
  let f (Range kL kR) c = (:) (c, kL, kR)
  in Test (==) (\w -> g w f []) (\w -> No.foldrR w f [])



foldrRangeT, foldrRangeT' :: IdT Range [(Color, Word, Word)]
foldrRangeT  = foldrRangeT_ Zebra.foldrRange
foldrRangeT' = foldrRangeT_ Zebra.foldrRange'

foldrRangeT_
  :: (forall x. Range -> (Range -> Color -> x -> x) -> x -> Zebra -> x)
  -> IdT Range [(Color, Word, Word)]
foldrRangeT_ g =
  let f (Range kL kR) c = (:) (c, kL, kR)
  in Test (==) (\w -> g w f []) (\w -> No.foldrRange w f [])



unionT :: Color -> TreeT (Zebra, NoSet)
unionT c = Test treeEq (Zebra.union c . fst) (No.union c . snd)

intersectionT :: Color -> TreeT (Zebra, NoSet)
intersectionT c = Test treeEq (Zebra.intersection c . fst) (No.intersection c . snd)

disjointT :: Color -> IdT (Zebra, NoSet) Bool
disjointT c = Test (==) (Zebra.disjoint c . fst) (No.disjoint c . snd)



differenceT :: Color -> TreeT (Zebra, NoSet)
differenceT c = Test treeEq (Zebra.difference c . fst)
                            (No.difference c . snd)

symmetricDifferenceT :: Color -> TreeT (Zebra, NoSet)
symmetricDifferenceT c = Test treeEq (Zebra.symmetricDifference c . fst)
                                     (No.symmetricDifference c . snd)



compareT :: Color -> IdT (Zebra, NoSet) No.PartialOrdering
compareT c = Test (==) (Zebra.compare c . fst) (No.compare c . snd)



test :: Spec
test = do
  describe "Single-key" $ do
    it "lookup"           $ run unary1_ lookupT

  describe "Left" $ do
    it "monoL"            $ run unary1_ monoLT
    it "sizeL"            $ run unary1  sizeLT
    it "lookupL"          $ run unary1  lookupLT
    it "findL"            $ run unary1  findLT
    it "fillL"            $ run unary1  fillLT
    it "foldlL"           $ run unary1_ foldlLT
    it "foldlL'"          $ run unary1_ foldlLT'
    it "foldrL"           $ run unary1_ foldrLT
    it "foldrL'"          $ run unary1_ foldrLT'

  describe "Right" $ do
    it "monoR"            $ run unary1_ monoRT
    it "sizeR"            $ run unary1  sizeRT
    it "lookupR"          $ run unary1  lookupRT
    it "findR"            $ run unary1  findRT
    it "fillR"            $ run unary1  fillRT
    it "foldlR"           $ run unary1_ foldlRT
    it "foldlR'"          $ run unary1_ foldlRT'
    it "foldrR"           $ run unary1_ foldrRT
    it "foldrR'"          $ run unary1_ foldrRT'

  describe "Range" $ do
    it "monoRange"        $ run unary2_ monoRangeT
    it "sizeRange"        $ run unary2  sizeRangeT
    it "fillRange"        $ run unary2  fillRangeT
    it "foldlRange"       $ run unary2_ foldlRangeT
    it "foldlRange'"      $ run unary2_ foldlRangeT'
    it "foldrRange"       $ run unary2_ foldrRangeT
    it "foldrRange'"      $ run unary2_ foldrRangeT'

  describe "Full-tree" $ do
    it "Mono"             $ run unary0 monoT
    it "size/White"       $ run unary0 (sizeT White)
    it "size/Black"       $ run unary0 (sizeT Black)
    it "foldl"            $ run unary0 foldlT
    it "foldl'"           $ run unary0 foldlT'
    it "foldr"            $ run unary0 foldrT
    it "foldr'"           $ run unary0 foldrT'

  describe "Merge" $ do
    it "complement"                 $ run unary0  complementT
    it "union/White"                $ run binaryL (unionT White)
    it "union/Black"                $ run binaryL (unionT Black)
    it "disjoint/White"             $ run binaryL (disjointT White)
    it "disjoint/Black"             $ run binaryL (disjointT Black)
    it "intersection/White"         $ run binaryL (intersectionT White)
    it "intersection/Black"         $ run binaryL (intersectionT Black)
    it "difference/White"           $ run binaryL (differenceT White)
    it "difference/Black"           $ run binaryL (differenceT Black)
    it "symmetricDifference/White"  $ run binaryL (symmetricDifferenceT White)
    it "symmetricDifference/Black"  $ run binaryL (symmetricDifferenceT Black)

    it "compare/incomparable/White" $ run binaryL        (compareT White)
    it "compare/incomparable/Black" $ run binaryL        (compareT Black)
    it "compare/equal/White"        $ run equal          (compareT White)
    it "compare/equal/Black"        $ run equal          (compareT Black)
    it "compare/subset/White"       $ run (subset White) (compareT White)
    it "compare/subset/Black"       $ run (subset Black) (compareT Black)
    it "compare/superset/White"     $ run (subset Black) (compareT White)
    it "compare/superset/Black"     $ run (subset White) (compareT Black)
