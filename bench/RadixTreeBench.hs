{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Arrow
import Control.DeepSeq
import Control.Exception

import Data.Foldable

import qualified Data.ByteString.Short as BSS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

import qualified Data.HashTable.IO as HT

import Gauge

import qualified Data.RadixTree.Internal as RT

main :: IO ()
main = do
  let config = defaultConfig
        { resamples   = 10000
        , displayMode = Condensed
        , rerunsLimit = 1
        }

  contents <- TLIO.readFile "/tmp/tags-ebac8dcc87fd1f1b1e7016d6585549309e3c5016-haskell-mode"
  let tags :: [TL.Text]
      tags = filter (not . TL.null) $ map (head . TL.splitOn "\t") $ drop 1 $ TL.lines contents

      decodeBS = TE.encodeUtf8 . TL.toStrict
      decode = BSS.toShort . decodeBS

      tags' :: [BSS.ShortByteString]
      tags' = map decode tags

      tags'' :: [(BSS.ShortByteString, ())]
      tags'' = map (id &&& const ()) tags'

      tagsRev'' :: [(BSS.ShortByteString, ())]
      tagsRev'' = map ((BSS.pack . reverse . BSS.unpack) &&& const ()) tags'

      -- tagsBS :: [(BS.ByteString, ())]
      -- tagsBS = map (decodeBS &&& const ()) tags

      queriesPresent :: [BSS.ShortByteString]
      queriesPresent = tags' ++ map (BSS.pack . reverse . BSS.unpack) tags'

      queriesMissing :: [BSS.ShortByteString]
      queriesMissing = map (BSS.pack . reverse . BSS.unpack) tags'

      queriesBoth :: [BSS.ShortByteString]
      queriesBoth = tags' ++ map (BSS.pack . reverse . BSS.unpack) tags'

  evaluate $ rnf tags'
  evaluate $ rnf tags''
  evaluate $ rnf tagsRev''
  evaluate $ rnf queriesPresent
  evaluate $ rnf queriesMissing
  evaluate $ rnf queriesBoth

  let radixTree    = RT.fromList tags''
      radixTreeRev = RT.fromList tagsRev''
      treeMap      = M.fromList  tags''
      treeMapRev   = M.fromList  tagsRev''
      hashMap      = HM.fromList tags''
      hashMapRev   = HM.fromList tagsRev''

  evaluate $ rnf radixTree
  evaluate $ rnf radixTreeRev
  evaluate $ rnf treeMap
  evaluate $ rnf treeMapRev
  evaluate $ rnf hashMap
  evaluate $ rnf hashMapRev

  (basic  :: HT.BasicHashTable  BSS.ShortByteString ()) <- HT.new
  -- (linear :: HT.LinearHashTable BSS.ShortByteString ()) <- HT.new
  (cuckoo :: HT.CuckooHashTable BSS.ShortByteString ()) <- HT.new
  for_ tags'' $ \(k, v) -> do
    HT.insert basic  k v
    -- HT.insert linear k v
    HT.insert cuckoo k v

  defaultMainWith config
    [ bgroup "creation"
      [ bench "Data.RadixTree"  $ nf RT.fromList tags''
      , bench "Data.Map"        $ nf M.fromList tags''
      , bench "Data.HashMap"    $ nf HM.fromList tags''
      , bench "BasicHashTable"  $ nfIO $ do
          (ht :: HT.BasicHashTable  BSS.ShortByteString ()) <- HT.new
          for_ tags'' $ \(k, v) -> HT.insert ht k v
      -- , bench "LinearHashTable"  $ nfIO $ do
      --     (ht :: HT.LinearHashTable BSS.ShortByteString ()) <- HT.new
      --     for_ tags'' $ \(k, v) -> HT.insert ht k v
      , bench "CuckooHashTable"  $ nfIO $ do
          (ht :: HT.CuckooHashTable BSS.ShortByteString ()) <- HT.new
          for_ tags'' $ \(k, v) -> HT.insert ht k v
      ]
    , bgroup "lookup"
        [ bgroup "present"
          [ bench "Data.RadixTree"  $ nf (map (`RT.lookup` radixTree)) queriesPresent
          , bench "Data.Map"        $ nf (map (`M.lookup`  treeMap))   queriesPresent
          , bench "Data.HashMap"    $ nf (map (`HM.lookup` hashMap))   queriesPresent
          , bench "BasicHashTable"  $ nfIO $ traverse (HT.lookup basic)  queriesPresent
          -- , bench "LinearHashTable" $ nfIO $ traverse (HT.lookup linear) queriesPresent
          , bench "CuckooHashTable" $ nfIO $ traverse (HT.lookup cuckoo) queriesPresent
          ]
        , bgroup "missing"
          [ bench "Data.RadixTree"  $ nf (map (`RT.lookup` radixTree)) queriesMissing
          , bench "Data.Map"        $ nf (map (`M.lookup`  treeMap))   queriesMissing
          , bench "Data.HashMap"    $ nf (map (`HM.lookup` hashMap))   queriesMissing
          , bench "BasicHashTable"  $ nfIO $ traverse (HT.lookup basic)  queriesMissing
          -- , bench "LinearHashTable" $ nfIO $ traverse (HT.lookup linear) queriesMissing
          , bench "CuckooHashTable" $ nfIO $ traverse (HT.lookup cuckoo) queriesMissing
          ]
        , bgroup "both"
          [ bench "Data.RadixTree"  $ nf (map (`RT.lookup` radixTree)) queriesBoth
          , bench "Data.Map"        $ nf (map (`M.lookup`  treeMap))   queriesBoth
          , bench "Data.HashMap"    $ nf (map (`HM.lookup` hashMap))   queriesBoth
          , bench "BasicHashTable"  $ nfIO $ traverse (HT.lookup basic)  queriesBoth
          -- , bench "LinearHashTable" $ nfIO $ traverse (HT.lookup linear) queriesBoth
          , bench "CuckooHashTable" $ nfIO $ traverse (HT.lookup cuckoo) queriesBoth
          ]
        ]
    , bgroup "keys"
      [ bench "Data.RadixTree" $ nf RT.keys radixTree
      , bench "Data.Map"       $ nf M.keys treeMap
      , bench "Data.HashMap"   $ nf HM.keys hashMap
      ]
    , bgroup "toList"
      [ bench "Data.RadixTree" $ nf RT.toList radixTree
      , bench "Data.Map"       $ nf M.toList treeMap
      , bench "Data.HashMap"   $ nf HM.toList hashMap
      ]
    , bgroup "union"
      [ bench "Data.RadixTree" $ nf (uncurry RT.union) (radixTree, radixTreeRev)
      , bench "Data.Map"       $ nf (uncurry M.union) (treeMap, treeMapRev)
      , bench "Data.HashMap"   $ nf (uncurry HM.union) (hashMap, hashMapRev)
      ]
    ]
