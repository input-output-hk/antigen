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

  -- * AntiGen combinators
  fickleNum,
  fickleBool,
  fickleTry,
  fickleTryGen,
  antiChoose,
  antiChooseBounded,
  antiPositive,
  antiNonPositive,
  antiNegative,
  antiNonNegative,
) where

import Control.Monad (join)
import System.Random (Random)
import Test.AntiGen.Internal
import Test.QuickCheck (
  Arbitrary (..),
  Gen,
  Negative (..),
  NonNegative (..),
  NonPositive (..),
  NonZero (..),
  Positive (..),
 )
import Test.QuickCheck.GenT (MonadGen (..), frequency, suchThat)

-- | Returns the provided number. If negated, returns a value that is not equal
-- to the provided number.
fickleNum :: (Eq a, Num a, Arbitrary a) => a -> AntiGen a
fickleNum n = pure n |! ((n +) . getNonZero <$> arbitrary)

-- | Returns the provided `Bool`. If negated, returns the negation of that
-- `Bool`.
fickleBool :: Bool -> AntiGen Bool
fickleBool b = pure b |! pure (not b)

-- | In the positive case generates a value from the first range. In the
-- negative case generates a value from the second range excluding the first
-- range.
--
-- Note: The second range must not be a subset of the first range!
antiChoose :: (Integral a, Random a) => (a, a) -> (a, a) -> AntiGen a
antiChoose rng@(lo, hi) (boundLo, boundHi) =
  choose rng
    |! frequency
      ( mconcat
          [ [ (fromIntegral $ lo - boundLo, choose rngLo)
            | lo > boundLo
            ]
          , [ (fromIntegral $ boundHi - hi, choose rngHi)
            | boundHi > hi
            ]
          ]
      )
  where
    rngLo = (boundLo, pred lo)
    rngHi = (succ hi, boundHi)

-- | Generates a value from the range. If negated, returns a random value
-- outside the range between `minBound` and `maxBound`.
antiChooseBounded :: (Integral a, Random a, Bounded a) => (a, a) -> AntiGen a
antiChooseBounded rng = antiChoose rng (minBound, maxBound)

-- | Returns the provided value unless negated, in which case it generates an
-- arbitrary value that is different from the provided value. It uses `suchThat`,
-- so using it on small types might end up discarding many values.
fickleTry :: (Eq a, Arbitrary a) => a -> AntiGen a
fickleTry a = fickleTryGen a arbitrary

-- | Returns the provided value unless negated, in which case it uses the
-- generator to generate a random value that is different from the provided
-- value. It uses `suchThat`, so using it on small types might end up 
-- discarding many values.
fickleTryGen :: Eq a => a -> Gen a -> AntiGen a
fickleTryGen a gen = pure a |! (gen `suchThat` (/= a))

-- | Negatable generator for positive numbers
antiPositive :: (Num a, Ord a, Arbitrary a) => AntiGen a
antiPositive = (getPositive <$> arbitrary) |! (getNonPositive <$> arbitrary)

-- | Negatable generator for non-positive numbers
antiNonPositive :: (Num a, Ord a, Arbitrary a) => AntiGen a
antiNonPositive = (getNonPositive <$> arbitrary) |! (getPositive <$> arbitrary)

-- | Negatable generator for negative numbers
antiNegative :: (Num a, Ord a, Arbitrary a) => AntiGen a
antiNegative = (getNegative <$> arbitrary) |! (getNonNegative <$> arbitrary)

-- | Negatable generator for non-negative numbers
antiNonNegative :: (Num a, Ord a, Arbitrary a) => AntiGen a
antiNonNegative = (getNonNegative <$> arbitrary) |! (getNegative <$> arbitrary)

-- | Create an `AntiGen` from a positive and a negative `AntiGen` generator
(||!) :: AntiGen a -> AntiGen a -> AntiGen a
a ||! b = join $ pure a |! pure b
