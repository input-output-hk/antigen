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
  showPartialGen,
) where

import Control.Monad ((<=<))
import Control.Monad.Free.Church (F (..), MonadFree (..))
import Control.Monad.Free.Class (wrapT)
import Control.Monad.Trans (MonadTrans (..))
import Debug.Trace (trace)
import Test.QuickCheck (Gen)
import Test.QuickCheck.GenT (GenT (..), MonadGen (..), runGenT)

data BiGen next where
  BiGen :: Show t => Gen t -> Maybe (Gen t) -> (t -> next) -> BiGen next

instance Functor BiGen where
  fmap f (BiGen p n c) = BiGen p n $ f . c

newtype AntiGen a = AntiGen (F BiGen a)
  deriving (Functor, Applicative, Monad)

always :: Show a => Gen a -> AntiGen a
always g = AntiGen $ F $ \p b -> b $ BiGen g Nothing p

sometimes :: Show a => Gen a -> Gen a -> AntiGen a
sometimes pos neg = AntiGen $ F $ \p b -> b $ BiGen pos (Just neg) p

data DecisionPoint next where
  DecisionPoint ::
    Show t =>
    { dpValue :: t
    , dpPositiveGen :: Gen t
    , dpNegativeGen :: Maybe (Gen t)
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
  wrapT $ DecisionPoint value pos mNeg c

countDecisionPoints :: PartialGen a -> Int
countDecisionPoints (PartialGen (F m)) = m (const 0) $ \dp@DecisionPoint {..} ->
  case dpNegativeGen of
    Just _ -> succ $ continue dp
    Nothing -> continue dp

regenerate :: PartialGen a -> Gen (PartialGen a)
regenerate (PartialGen (F m)) = m (pure . pure) $ \(DecisionPoint {..}) -> do
  value <- dpPositiveGen
  runGenT . wrapGenT $ DecisionPoint value dpPositiveGen dpNegativeGen $ \x -> do
    c <- liftGen $ dpContinuation x
    c' <- liftGen $ regenerate c
    lift c'

zap :: PartialGen a -> Gen (PartialGen a)
zap p
  | let maxDepth = countDecisionPoints p
  , maxDepth > 0 = do
      cutoffDepth <- choose (0, maxDepth - 1)
      let
        skip d DecisionPoint {..} = do
          runGenT . wrapGenT $ DecisionPoint dpValue dpPositiveGen dpNegativeGen $ \x -> do
            c <- liftGen $ dpContinuation x
            pg' <- liftGen $ go c (d - 1)
            lift pg'

        go :: PartialGen a -> Int -> Gen (PartialGen a)
        go (PartialGen (F m)) d = m (pure . pure) $ \case
          dp@DecisionPoint {..}
            | 0 <- d
            , Just neg <- dpNegativeGen -> do
                value <- liftGen neg
                runGenT . wrapGenT . DecisionPoint value neg Nothing $ \x -> do
                  c <- liftGen $ dpContinuation x
                  lift =<< liftGen (regenerate c)
            | otherwise -> skip d dp
      go p cutoffDepth
  | otherwise = pure p

-- TODO This should probably zap all the nodes in one go, otherwise we might fix
-- some values to a constant value even if they would be affected by some
-- changing value in an earlier node
zapNTimes :: Show a => Int -> PartialGen a -> Gen (PartialGen a)
zapNTimes n x
  | n <= 0 = pure x
  | otherwise = zapNTimes (n - 1) =<< fmap (\y -> trace (info y) y) (zap x)
  where
    info y =
      "\nBefore:\n" <> showPartialGen x <> "\nAfter:\n" <> showPartialGen y

evalPartial :: PartialGen a -> a
evalPartial (PartialGen (F m)) = m id continue

runAntiGen :: Show a => Int -> AntiGen a -> Gen a
runAntiGen n = fmap evalPartial <$> zapNTimes n <=< evalToPartial

showPartialGen :: Show a => PartialGen a -> String
showPartialGen (PartialGen (F m)) = m show $ \dp@DecisionPoint {..} ->
  "Generated value: " <> show dpValue <> "\n" <> continue dp
