{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE BangPatterns #-}

module Main (main) where

import qualified Data.HyperLogLogPlus.Benchmarks as B

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

  defaultMain
    [ bgroup "HLL 12 2048"
      [ bgroup "HLL semigroup"
        [ bgroup "foldr"  [ B.benchSemigroup 10000,           B.benchSemigroup 50000]
        , bgroup "foldl'" [ B.benchSemigroupLStrict 10000,    B.benchSemigroupLStrict 50000]
        ]
      , bgroup "HLL size"      [ B.benchSize 10000 hll1,           B.benchSize 50000 hll2 ]
      , bgroup "HLL intersect" [ B.benchIntersect 10000 hll3 hll4, B.benchIntersect 50000 hll5 hll6 ]
      ]
    , bgroup "HLL 12 0"
      [ bgroup "HLL semigroup"
        [ bgroup "foldr"  [ B.benchSemigroup0 10000,           B.benchSemigroup0 50000]
        , bgroup "foldl'" [ B.benchSemigroupLStrict0 10000,    B.benchSemigroupLStrict0 50000]
        ]
      ]
    ]

setupHLL :: Int -> Int -> B.HLL
setupHLL from to = foldr insert mempty (map show [from .. to])
