{-# LANGUAGE DataKinds #-}

module Data.HyperLogLogPlus.Benchmarks
  (
    benchInserts
  , benchInsertsLStrict
  , benchInsertHashes
  , benchInsertBatched
  , benchSemigroupTwo
  , benchSize
  , benchIntersect
  , HLL
  -- * Zero-minhash variants.
  , benchInserts0
  , benchInsertsLStrict0
  , benchInsertBatched0
  , benchSemigroupTwo0
  , HLL0
  ) where

import Data.Digest.Murmur64 (Hash64)
import Data.Foldable (foldl')
import Data.HyperLogLogPlus
import Data.Word
import Data.Semigroup

import Criterion            (Benchmark, bench, nf, env)


type HLL = HyperLogLogPlus 12 2048
type HLL0 = HyperLogLogPlus 12 0

benchInserts :: Int -> Benchmark
benchInserts n = bench (show n) $ nf f n
  where f n = size (foldr insert mempty (map show [1 .. n]) :: HLL)

benchInsertHashes :: Int -> [Hash64] -> Benchmark
benchInsertHashes n hs = bench (show n) $ nf f hs
  where f hs = size (foldr insertHash mempty hs :: HLL)

benchInsertsLStrict :: Int -> Benchmark
benchInsertsLStrict n = bench (show n) $ nf f n
  where f n = size (foldl' (flip insert) mempty (map show [1 .. n]) :: HLL)

benchInsertBatched :: Int -> Benchmark
benchInsertBatched n = bench (show n) $ nf f n
  where f n = size (batchInsert (map show [1 .. n]) mempty :: HLL)

benchSemigroupTwo :: Int -> HLL -> HLL -> Benchmark
benchSemigroupTwo n hll1 hll2 = bench (show n) $ nf f (hll1, hll2)
  where f :: (HLL, HLL) -> Word64
        f (l, r) = size (l <> r)

benchSize :: Int -> HLL -> Benchmark
benchSize n hll = bench (show n) $ nf size hll
  where f :: HLL -> Word64
        f = size

benchIntersect :: Int -> HLL -> HLL -> Benchmark
benchIntersect n hll1 hll2 = bench (show n) $ nf f (hll1, hll2)
  where f :: (HLL, HLL) -> Word64
        f (l, r) = intersection [l, r]

benchInserts0 :: Int -> Benchmark
benchInserts0 n = bench (show n) $ nf f n
  where f n = size (foldr insert mempty (map show [1 .. n]) :: HLL0)

benchInsertsLStrict0 :: Int -> Benchmark
benchInsertsLStrict0 n = bench (show n) $ nf f n
  where f n = size (foldl' (flip insert) mempty (map show [1 .. n]) :: HLL0)

benchInsertBatched0 :: Int -> Benchmark
benchInsertBatched0 n = bench (show n) $ nf f n
  where f n = size (batchInsert (map show [1 .. n]) mempty :: HLL0)

benchSemigroupTwo0 :: Int -> HLL0 -> HLL0 -> Benchmark
benchSemigroupTwo0 n hll1 hll2 = bench (show n) $ nf f (hll1, hll2)
  where f :: (HLL0, HLL0) -> Word64
        f (l, r) = size (l <> r)


