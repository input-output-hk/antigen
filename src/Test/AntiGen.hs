{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Test.AntiGen (
  AntiGen,
  ZapResult (..),
  (|!),
  (#!),
  (||!),
  withAnnotation,
  runAntiGen,
  zapAntiGen,
  zapAntiGenResult,
  prettyZapResult,
  scaleWeight,
  reweigh,

  -- * Normalized monad combinators
  replicateMNorm,
  traverseNorm,

  -- * AntiGen combinators
  faultyNum,
  faultyNumRange,
  faultyBool,
  faultyTry,
  faultyTryGen,
  antiChoose,
  antiChooseBounded,
  antiPositive,
  antiNonPositive,
  antiNegative,
  antiNonNegative,
  antiJust,
  antiNonEmpty,
  antiSamePair,
  antiDistinctPair,
  antiSort,
  antiVectorOfUnique,
  antiVectorOfUniqueBy,
  antiVectorOfUniqueOn,
) where

import Control.Monad (join, replicateM)
import Data.Function (on)
import Data.List (sort)
import System.Random (Random)
import Test.AntiGen.Internal (
  AntiGen,
  ZapResult (..),
  prettyZapResult,
  reweigh,
  runAntiGen,
  scaleWeight,
  withAnnotation,
  zapAntiGen,
  zapAntiGenResult,
  (#!),
  (|!),
 )
import Test.QuickCheck (
  Arbitrary (..),
  Negative (..),
  NonNegative (..),
  NonPositive (..),
  NonZero (..),
  Positive (..),
  discard,
 )
import Test.QuickCheck.GenT (MonadGen (..), listOf1, oneof, suchThat, vectorOf)

-- | Returns the provided number.
--
-- Negative: returns a value that is not equal to the provided number.
faultyNum :: (Eq a, Num a, Arbitrary a) => a -> AntiGen a
faultyNum n = pure n |! ((n +) . getNonZero <$> arbitrary)

faultyNumRange :: (Random a, Eq a) => a -> (a, a) -> AntiGen a
faultyNumRange n rng = pure n |! (choose rng `suchThat` (/= n))

-- | Returns the provided `Bool`.
--
-- Negative: returns the negation of that `Bool`.
faultyBool :: Bool -> AntiGen Bool
faultyBool b = pure b |! pure (not b)

-- | Generates a value from the first range.
--
-- Negative: Generates a value from the second range excluding the first range.
antiChoose :: (Integral a, Random a) => (a, a) -> (a, a) -> AntiGen a
antiChoose rng@(lo, hi) (boundLo, boundHi)
  | lo > boundLo || boundHi > hi =
      choose rng
        |! oneof
          ([choose rngLo | lo > boundLo] <> [choose rngHi | boundHi > hi])
  | otherwise = choose rng
  where
    rngLo = (boundLo, pred lo)
    rngHi = (succ hi, boundHi)

-- | Generates a value from the range.
--
-- Negative: Returns a random value outside the range between `minBound` and
-- `maxBound`.
antiChooseBounded :: (Integral a, Random a, Bounded a) => (a, a) -> AntiGen a
antiChooseBounded rng = antiChoose rng (minBound, maxBound)

-- | Returns the provided value
--
-- Negative: Generates an arbitrary value that is different from the provided
-- value.
--
-- Warning: It uses `suchThat`, so using it on small types might end up
-- discarding many values.
faultyTry :: (Eq a, Arbitrary a) => a -> AntiGen a
faultyTry a = faultyTryGen a $ liftGen arbitrary

-- | Returns the provided value
--
-- Negative: Use the generator to generate a random value that is different
-- from the provided value.
--
-- Warning: It uses `suchThat`, so using it on small types might end up
-- discarding many values.
faultyTryGen :: Eq a => a -> AntiGen a -> AntiGen a
faultyTryGen a gen = pure a ||! (gen `suchThat` (/= a))

-- | Returns a positive number
--
-- Negative: Returns a non-positive number
antiPositive :: (Num a, Ord a, Arbitrary a) => AntiGen a
antiPositive = (getPositive <$> arbitrary) |! (getNonPositive <$> arbitrary)

-- | Returns a non-positive number
--
-- Negative: Returns a positive number
antiNonPositive :: (Num a, Ord a, Arbitrary a) => AntiGen a
antiNonPositive = (getNonPositive <$> arbitrary) |! (getPositive <$> arbitrary)

-- | Returns a negative number
--
-- Negative: Returns a non-negative number
antiNegative :: (Num a, Ord a, Arbitrary a) => AntiGen a
antiNegative = (getNegative <$> arbitrary) |! (getNonNegative <$> arbitrary)

-- | Returns a non-negative number
--
-- Negative: Returns a negative number
antiNonNegative :: (Num a, Ord a, Arbitrary a) => AntiGen a
antiNonNegative = (getNonNegative <$> arbitrary) |! (getNegative <$> arbitrary)

-- | Returns `Just x`
--
-- Negative: Returns `Nothing`
antiJust :: a -> AntiGen (Maybe a)
antiJust x = pure (Just x) ||! pure Nothing

-- | Returns a non-empty list
--
-- Negative: Generate an empty list
antiNonEmpty :: AntiGen a -> AntiGen [a]
antiNonEmpty x = listOf1 x ||! pure []

-- | Generate a pair with equal values
--
-- Negative: Generates a pair of distinct values
antiSamePair :: (Arbitrary a, Num a, Eq a) => AntiGen (a, a)
antiSamePair =
  ((\x -> (x, x)) <$> arbitrary)
    |! ( do
           x <- arbitrary
           NonZero s <- arbitrary
           return (x, x + s)
       )

-- | Generates a pair (x, y) where x /= y.
--
-- Negative: Generates a pair (x, y) where x == y.
antiDistinctPair :: (Num a, Arbitrary a, Eq a) => AntiGen (a, a)
antiDistinctPair =
  ( do
      x <- arbitrary
      NonZero s <- arbitrary
      return (x, x + s)
  )
    |! ( do
           x <- arbitrary
           return (x, x)
       )

-- | Returns the sorted list.
--
-- Negative: Swaps two distinct elements to break sort order.
-- If all elements are equal, there is no negative case.
antiSort :: Ord a => [a] -> AntiGen [a]
antiSort (sort -> sorted)
  | allEqual sorted = pure sorted
  | otherwise =
      let
        -- pick two nonequal elements and swap their places, making the list no
        -- longer sorted
        permute ys = do
          i <- choose (0, length ys - 1)
          let
            vi = ys !! i
            others = [(k, v) | (k, v) <- zip [0 ..] ys, v /= vi]
          j <- choose (0, length others - 1)
          let
            (j', vj) = others !! j
            swap k v
              | k == i = vj
              | k == j' = vi
              | otherwise = v
          pure $ zipWith swap [0 :: Int ..] ys
       in
        pure sorted |! permute sorted
  where
    allEqual [] = True
    allEqual (y : ys) = all (== y) ys

-- | Generate a list of @n@ pairwise-distinct elements. Discards the example if
-- the underlying generator could not produce enough distinct elements within
-- the per-element retry budget.
--
-- Negative: one element is overwritten with a copy of another, so the list
-- contains a duplicate pair
antiVectorOfUnique :: Eq a => Int -> AntiGen a -> AntiGen [a]
antiVectorOfUnique = antiVectorOfUniqueBy (==)

-- | Like 'antiVectorOfUnique', but compares elements by a key projection.
antiVectorOfUniqueOn :: Eq b => (a -> b) -> Int -> AntiGen a -> AntiGen [a]
antiVectorOfUniqueOn key = antiVectorOfUniqueBy ((==) `on` key)

-- | Like 'antiVectorOfUnique', but takes a user-supplied equivalence relation.
-- The relation must be reflexive, otherwise the negative case is not
-- guaranteed to contain a duplicate.
antiVectorOfUniqueBy :: (a -> a -> Bool) -> Int -> AntiGen a -> AntiGen [a]
antiVectorOfUniqueBy eq n gen
  | n <= 1 = vectorOf n gen
  | otherwise = do
      disallowDuplicates <- faultyBool True
      let
        triesPerElement = 10 :: Int
        go _ 0 _ = discard
        go m tries elems
          | m > 0 = do
              x <- gen
              if any (eq x) elems
                then go m (tries - 1) elems
                else go (m - 1) triesPerElement (x : elems)
          | otherwise = pure elems
      xs <- go n triesPerElement []
      if disallowDuplicates
        then pure xs
        else do
          -- copy the element at src over a distinct position dst
          src <- choose (0, n - 1)
          offset <- choose (1, n - 1)
          let dst = (src + offset) `mod` n
          pure [if k == dst then xs !! src else x | (k, x) <- zip [0 :: Int ..] xs]

-- | Like `traverse`, but normalizes the weights of the elements
traverseNorm :: (a -> AntiGen a) -> [a] -> AntiGen [a]
traverseNorm f l = scaleWeight (/ fromIntegral (length l)) $ traverse f l

-- | Create an `AntiGen` from a positive and a negative `AntiGen` generator
(||!) :: AntiGen a -> AntiGen a -> AntiGen a
a ||! b = join $ pure a |! pure b

-- | Like 'replicateM', but normalizes the weights of the elements
--
-- The total weight of the list becomes the average weight of its elements,
-- rather than the sum. This prevents longer lists from having a
-- disproportionately higher chance of being zapped.
replicateMNorm :: Int -> AntiGen a -> AntiGen [a]
replicateMNorm n = replicateM n . scaleWeight (/ fromIntegral n)

infixl 6 ||!
