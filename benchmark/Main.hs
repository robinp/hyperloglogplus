{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE BangPatterns #-}

module Main (main) where

import qualified Data.HyperLogLogPlus.Benchmarks as B

import           Data.Digest.Murmur64 (hash64)
import           Data.Semigroup
import           Data.HyperLogLogPlus

import           Criterion.Main                  (bgroup, defaultMain)

main :: IO ()
main = do
  let !hll1 = setupHLL 1 10000
  let !hll2 = setupHLL 1 50000

  let !hll3 = setupHLL 1 7000
  let !hll4 = setupHLL 5000 10000

  let !hll5 = setupHLL 1 35000
  let !hll6 = setupHLL 30000 50000

  let !hll03 = setupHLL0 1 7000
  let !hll04 = setupHLL0 5000 10000

  let !hll05 = setupHLL0 1 35000
  let !hll06 = setupHLL0 30000 50000

  let !hs10k = let xs = map hash64 [1..(10000::Int)] in length xs `seq` xs
  let !hs50k = let xs = map hash64 [1..(50000::Int)] in length xs `seq` xs


  defaultMain
    [ bgroup "HLL 12 2048"
      [ bgroup "inserts"
        [ bgroup "foldr"  [ B.benchInserts 10000,              B.benchInserts 50000]
        , bgroup "foldl'" [ B.benchInsertsLStrict 10000,       B.benchInsertsLStrict 50000]
        , bgroup "hashes" [ B.benchInsertHashes 10000 hs10k,   B.benchInsertHashes 50000 hs50k]
        ]
      , bgroup "semigroup"
        [ bgroup "two"    [ B.benchSemigroupTwo 10000 hll3 hll4, B.benchSemigroupTwo 50000 hll5 hll6]
        ]
      , bgroup "size"      [ B.benchSize 10000 hll1,           B.benchSize 50000 hll2 ]
      , bgroup "intersect" [ B.benchIntersect 10000 hll3 hll4, B.benchIntersect 50000 hll5 hll6 ]
      ]
    , bgroup "HLL 12 0"
      [ bgroup "inserts"
        [ bgroup "foldr"  [ B.benchInserts0 10000,           B.benchInserts0 50000]
        , bgroup "foldl'" [ B.benchInsertsLStrict0 10000,    B.benchInsertsLStrict0 50000]
        ]
      , bgroup "semigroup"
        [ bgroup "two"    [ B.benchSemigroupTwo0 10000 hll03 hll04, B.benchSemigroupTwo0 50000 hll05 hll06]
        ]
      ]
    ]

setupHLL :: Int -> Int -> B.HLL
setupHLL from to = foldr insert mempty (map show [from .. to])

setupHLL0 :: Int -> Int -> B.HLL0
setupHLL0 from to = foldr insert mempty (map show [from .. to])
