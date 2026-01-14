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
import Control.Monad.State.Strict (MonadState (..), StateT (..), evalStateT, modify)
import Control.Monad.Trans (MonadTrans (..))
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

wrapGenState :: (MonadFree f m, Functor f) => f (StateT Int (GenT m) a) -> StateT Int (GenT m) a
wrapGenState m = StateT $ \s -> GenT $ \g sz ->
  let eval (StateT x) =
        let GenT f = x s
         in f g sz
   in wrap $ eval <$> m

evalToPartial :: AntiGen a -> Gen (PartialGen a)
evalToPartial (AntiGen (F m)) = runGenT $ m pure $ \(BiGen pos mNeg c) -> do
  value <- liftGen pos
  wrapT $ DecisionPoint value pos mNeg c

countDecisionPoints :: PartialGen a -> Int
countDecisionPoints (PartialGen (F m)) = m (const 0) $ \dp@DecisionPoint {..} ->
  case dpNegativeGen of
    Just _ -> succ $ continue dp
    Nothing -> continue dp

zap :: PartialGen a -> Gen (PartialGen a)
zap p@(PartialGen (F m))
  | let maxDepth = countDecisionPoints p
  , maxDepth > 0 = do
      cutoffDepth <- choose (0, maxDepth - 1)
      runGenT . (`evalStateT` cutoffDepth) . m pure $ \dp@DecisionPoint {..} ->
        case dpNegativeGen of
          Just neg -> do
            d <- get
            modify pred
            case compare 0 d of
              EQ -> do
                -- Negate the generator
                value <- lift $ liftGen neg
                wrapGenState $ DecisionPoint value neg Nothing dpContinuation
              LT ->
                -- Continue
                wrapGenState dp
              GT -> do
                -- Regenerate
                value <- lift $ liftGen dpPositiveGen
                wrapGenState $ DecisionPoint value dpPositiveGen dpNegativeGen dpContinuation
          Nothing -> wrapGenState dp
  | otherwise = pure p

zapNTimes :: Show a => Int -> PartialGen a -> Gen (PartialGen a)
zapNTimes n x
  | n <= 0 = pure x
  | otherwise = zapNTimes (n - 1) =<< zap x

evalPartial :: PartialGen a -> a
evalPartial (PartialGen (F m)) = m id continue

runAntiGen :: Show a => Int -> AntiGen a -> Gen a
runAntiGen n = fmap evalPartial <$> zapNTimes n <=< evalToPartial

showPartialGen :: Show a => PartialGen a -> String
showPartialGen (PartialGen (F m)) = m show $ \dp@DecisionPoint {..} ->
  "Generated value: " <> show dpValue <> "\n" <> continue dp
