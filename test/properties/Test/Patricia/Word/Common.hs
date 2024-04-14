{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Patricia.Word.Common
  ( test
  ) where

import           Data.Patricia.Word.Strict (Openness (..))
import           Data.Patricia.Word.Strict.Unsafe (Interval (..))

import           Test.Hspec



deriving instance Eq Openness
deriving instance Eq Interval



test :: Spec
test = do
  describe "Interval" $ do
    it "(3,12]" $
      Interval Open 3 Closed 12 == Interval Open 3 Closed 12

    it "[12,3)" $
      Interval Closed 12 Open 3 == Interval Open 3 Closed 12

    it "[0,0]" $
      Interval Closed 0 Closed 0 == Interval Closed 0 Open 1

    it "[max,max]" $
      Interval Closed maxBound Closed maxBound
        == Interval Open (maxBound - 1) Closed maxBound

    it "(5,5]" $
      Interval Open 5 Closed 5 == Interval Open 5 Open 6

    it "(7,7)" $
      Interval Open 7 Open 7 == Interval Open 7 Open 8
