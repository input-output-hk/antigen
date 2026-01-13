{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Test.AntiGen (
  AntiGen,
  always,
  sometimes,
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
  , dpNegativeGen :: Gen t
  , dpContinuation :: t -> next
  }
  deriving (Functor)

newtype PartialGen t a = PartialGen (F (DecisionPoint t) a)
  deriving (Functor, Applicative, Monad, MonadFree (DecisionPoint t))

evalToSpec :: AntiGen t a -> Gen (PartialGen t a)
evalToSpec (AntiGen (F m)) = runGenT $ m pure $ \(BiGen pos mNeg c) -> do
  x <- liftGen pos
  case mNeg of
    Just neg -> GenT $ \g s -> PartialGen $ F $ \p b ->
      b $
        DecisionPoint x neg $
          (\(GenT f) -> let PartialGen (F m') = f g s in m' p b)
            . c
    Nothing -> c x

countDecisionPoints :: PartialGen t a -> Int
countDecisionPoints (PartialGen (F m)) = m (const 0) $ \DecisionPoint {..} ->
  1 + dpContinuation dpValue

evalPartial :: PartialGen t a -> a
evalPartial (PartialGen (F m)) = m id $ \DecisionPoint {..} -> dpContinuation dpValue
