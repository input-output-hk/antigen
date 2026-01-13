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
  runAntiGen,
) where

import Control.Monad ((<=<))
import Control.Monad.Free.Church (F (..), MonadFree (..))
import Control.Monad.Free.Class (wrapT)
import Control.Monad.Trans (MonadTrans (..))
import Test.QuickCheck (Gen)
import Test.QuickCheck.GenT (GenT (..), MonadGen (..), runGenT)

data BiGen next where
  BiGen :: Gen t -> Maybe (Gen t) -> (t -> next) -> BiGen next

instance Functor BiGen where
  fmap f (BiGen p n c) = BiGen p n $ f . c

newtype AntiGen a = AntiGen (F BiGen a)
  deriving (Functor, Applicative, Monad)

always :: Gen a -> AntiGen a
always g = AntiGen $ F $ \p b -> b $ BiGen g Nothing p

sometimes :: Gen a -> Gen a -> AntiGen a
sometimes pos neg = AntiGen $ F $ \p b -> b $ BiGen pos (Just neg) p

data DecisionPoint next where
  DecisionPoint ::
    { dpValue :: t
    , dpPositiveGen :: Gen t
    , dpNegativeGen :: Gen t
    , dpContinuation :: t -> next
    } ->
    DecisionPoint next

instance Functor DecisionPoint where
  fmap f (DecisionPoint v p n c) = DecisionPoint v p n $ f . c

continue :: DecisionPoint next -> next
continue DecisionPoint {..} = dpContinuation dpValue

newtype PartialGen a = PartialGen (F DecisionPoint a)
  deriving (Functor, Applicative, Monad, MonadFree DecisionPoint)

wrapGenT :: (MonadFree f m, Functor f) => f (GenT m a) -> GenT m a
wrapGenT m = GenT $ \g s -> wrap $ (\(GenT f) -> f g s) <$> m

evalToPartial :: AntiGen a -> Gen (PartialGen a)
evalToPartial (AntiGen (F m)) = runGenT $ m pure $ \(BiGen pos mNeg c) -> do
  value <- liftGen pos
  case mNeg of
    Just neg -> wrapT $ DecisionPoint value pos neg c
    Nothing -> c value

countDecisionPoints :: PartialGen a -> Int
countDecisionPoints (PartialGen (F m)) = m (const 0) $ succ . continue

regenerate :: PartialGen a -> Gen (PartialGen a)
regenerate (PartialGen (F m)) = runGenT $ m pure $ \(DecisionPoint {..}) -> do
  value <- liftGen dpPositiveGen
  wrapGenT $ DecisionPoint value dpPositiveGen dpNegativeGen dpContinuation

zap :: PartialGen a -> Gen (PartialGen a)
zap p
  | let maxDepth = countDecisionPoints p
  , maxDepth > 0 = do
      cutoffDepth <- choose (0, maxDepth - 1)
      let
        go :: PartialGen a -> Int -> Gen (PartialGen a)
        go (PartialGen (F m)) 0 = m (pure . pure) $ \DecisionPoint {..} -> do
          regenerate =<< dpContinuation =<< dpNegativeGen
        go (PartialGen (F m)) d = m (pure . pure) $ \DecisionPoint {..} -> do
          runGenT . wrapGenT $ DecisionPoint dpValue dpPositiveGen dpNegativeGen $ \x -> do
            c <- liftGen $ dpContinuation x
            pg' <- liftGen $ go c (d - 1)
            lift pg'
      go p cutoffDepth
  | otherwise = pure p

zapNTimes :: Int -> PartialGen a -> Gen (PartialGen a)
zapNTimes n x
  | n <= 0 = pure x
  | otherwise = zapNTimes (n - 1) =<< zap x

evalPartial :: PartialGen a -> a
evalPartial (PartialGen (F m)) = m id continue

runAntiGen :: Int -> AntiGen a -> Gen a
runAntiGen n = fmap evalPartial <$> zapNTimes n <=< evalToPartial
