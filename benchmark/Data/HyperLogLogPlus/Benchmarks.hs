{-# LANGUAGE DataKinds #-}

module Data.HyperLogLogPlus.Benchmarks
  (
    benchSemigroup
  , benchSemigroupLStrict
  , benchSize
  , benchIntersect
  , HLL
  -- * Zero-minhash variants.
  , benchSemigroup0
  , benchSemigroupLStrict0
  , HLL0
  ) where

import Data.Foldable (foldl')
import Data.HyperLogLogPlus
import Data.Word
import Data.Semigroup

import Criterion            (Benchmark, bench, nf, env)


type HLL = HyperLogLogPlus 12 2048
type HLL0 = HyperLogLogPlus 12 0

benchSemigroup :: Int -> Benchmark
benchSemigroup n = bench (show n) $ nf f n
  where f n = size (foldr insert mempty (map show [1 .. n]) :: HLL)

benchSemigroupLStrict :: Int -> Benchmark
benchSemigroupLStrict n = bench (show n) $ nf f n
  where f n = size (foldl' (flip insert) mempty (map show [1 .. n]) :: HLL)

benchSize :: Int -> HLL -> Benchmark
benchSize n hll = bench (show n) $ nf size hll
  where f :: HLL -> Word64
        f = size

benchIntersect :: Int -> HLL -> HLL -> Benchmark
benchIntersect n hll1 hll2 = bench (show n) $ nf f (hll1, hll2)
  where f :: (HLL, HLL) -> Word64
        f (l, r) = intersection [l, r]

benchSemigroup0 :: Int -> Benchmark
benchSemigroup0 n = bench (show n) $ nf f n
  where f n = size (foldr insert mempty (map show [1 .. n]) :: HLL0)

benchSemigroupLStrict0 :: Int -> Benchmark
benchSemigroupLStrict0 n = bench (show n) $ nf f n
  where f n = size (foldl' (flip insert) mempty (map show [1 .. n]) :: HLL0)
