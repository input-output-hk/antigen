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

module Test.AntiGen (
  AntiGen,
  (|!),
  (||!),
  runAntiGen,
  zapAntiGen,
  tryZapAntiGen,

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
) where

import Control.Monad (join)
import System.Random (Random)
import Test.AntiGen.Internal
import Test.QuickCheck (
  Arbitrary (..),
  Negative (..),
  NonNegative (..),
  NonPositive (..),
  NonZero (..),
  Positive (..),
 )
import Test.QuickCheck.GenT (MonadGen (..), listOf1, oneof, suchThat)

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

-- | Create an `AntiGen` from a positive and a negative `AntiGen` generator
(||!) :: AntiGen a -> AntiGen a -> AntiGen a
a ||! b = join $ pure a |! pure b
