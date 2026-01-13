{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.AntiGen (
  AntiGen,
  always,
  sometimes,
  countDecisionPoints,
  evalToPartial,
) where

import Control.Monad.Free.Church (F (..), MonadFree (..))
import Test.QuickCheck (Gen)
import Test.QuickCheck.GenT (GenT (..), MonadGen (..), runGenT)

data BiGen t next = BiGen (Gen t) (Maybe (Gen t)) (t -> next)
  deriving (Functor)

newtype AntiGen t a = AntiGen (F (BiGen t) a)
  deriving (Functor, Applicative, Monad)

always :: Gen a -> AntiGen a a
always g = AntiGen $ F $ \p b -> b $ BiGen g Nothing p

sometimes :: Gen a -> Gen a -> AntiGen a a
sometimes pos neg = AntiGen $ F $ \p b -> b $ BiGen pos (Just neg) p

data DecisionPoint t next = DecisionPoint
  { dpValue :: t
  , dpPositiveGen :: Gen t
  , dpNegativeGen :: Gen t
  , dpContinuation :: t -> next
  }
  deriving (Functor)

continue :: DecisionPoint t next -> next
continue DecisionPoint {..} = dpContinuation dpValue

newtype PartialGen t a = PartialGen (F (DecisionPoint t) a)
  deriving (Functor, Applicative, Monad, MonadFree (DecisionPoint t))

wrapGenT :: (MonadFree f m, Functor f) => f (GenT m a) -> GenT m a
wrapGenT m = GenT $ \g s -> wrap $ (\(GenT f) -> f g s) <$> m

evalToPartial :: AntiGen t a -> Gen (PartialGen t a)
evalToPartial (AntiGen (F m)) = runGenT $ m pure $ \(BiGen pos mNeg c) -> do
  value <- liftGen pos
  case mNeg of
    Just neg -> wrapGenT $ DecisionPoint value pos neg c
    Nothing -> c value

countDecisionPoints :: PartialGen t a -> Int
countDecisionPoints (PartialGen (F m)) = m (const 0) $ succ . continue

regenerate :: PartialGen t a -> Gen (PartialGen t a)
regenerate (PartialGen (F m)) = runGenT $ m pure $ \(DecisionPoint {..}) -> do
  value <- liftGen dpPositiveGen
  wrapGenT $ DecisionPoint value dpPositiveGen dpNegativeGen dpContinuation

zap :: PartialGen t a -> Gen (PartialGen t a)
zap p
  | let maxDepth = countDecisionPoints p
  , maxDepth > 0 = do
      cutoffDepth <- choose (0, maxDepth - 1)
      let
        go :: PartialGen t a -> Int -> Gen (PartialGen t a)
        go (PartialGen (F m)) 0 = m undefined undefined
        go (PartialGen (F m)) d = m (\x -> go (pure x) d) $ \dp -> do
          c <- continue dp
          go c (d - 1)
      go p cutoffDepth
  | otherwise = pure p

evalPartial :: PartialGen t a -> a
evalPartial (PartialGen (F m)) = m id continue
