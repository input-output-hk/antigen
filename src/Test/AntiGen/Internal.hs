{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.AntiGen.Internal (
  AntiGen,
  (|!),
  zapAntiGen,
  runAntiGen,
  evalToPartial,
  countDecisionPoints,
) where

import Control.Monad ((<=<))
import Control.Monad.Free.Church (F (..), MonadFree (..))
import Control.Monad.Free.Class (wrapT)
import Control.Monad.State.Strict (MonadState (..), StateT (..), evalStateT, modify)
import Control.Monad.Trans (MonadTrans (..))
import Test.QuickCheck (Gen)
import Test.QuickCheck.GenT (GenT (..), MonadGen (..), runGenT)

data BiGen next where
  BiGen :: Gen t -> Maybe (Gen t) -> (t -> next) -> BiGen next

instance Functor BiGen where
  fmap f (BiGen p n c) = BiGen p n $ f . c

newtype AntiGen a = AntiGen (F BiGen a)
  deriving (Functor, Applicative, Monad, MonadFree BiGen)

mapGen :: (forall x. Gen x -> Gen x) -> AntiGen a -> AntiGen a
mapGen f (AntiGen (F m)) = m pure $ \(BiGen pos neg c) ->
  wrap $ BiGen (f pos) (f <$> neg) c

instance MonadGen AntiGen where
  liftGen g = AntiGen $ F $ \p b -> b $ BiGen g Nothing p
  variant n = mapGen (variant n)
  sized f = AntiGen $ F $ \p b ->
    let
      pos = sized $ \sz ->
        let AntiGen (F m) = f sz
         in m pure $ \(BiGen ps _ c) -> ps >>= c
     in
      b $ BiGen pos Nothing p
  resize n = mapGen (resize n)
  choose = liftGen . choose

(|!) :: Gen a -> Gen a -> AntiGen a
pos |! neg = AntiGen $ F $ \p b -> b $ BiGen pos (Just neg) p

data DecisionPoint next where
  DecisionPoint ::
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
      let
        wrapGenState mm = StateT $ \s -> GenT $ \g sz ->
          let eval (StateT x) =
                let GenT f = x s
                 in f g sz
           in wrap $ eval <$> mm
      cutoffDepth <- choose (0, maxDepth - 1)
      runGenT . (`evalStateT` cutoffDepth) . m pure $ \dp@DecisionPoint {..} ->
        case dpNegativeGen of
          Just neg -> do
            d <- get
            modify pred
            if d == 0
              then do
                -- Negate the generator
                value <- lift $ liftGen neg
                wrapGenState $ DecisionPoint value neg Nothing dpContinuation
              else wrapGenState dp
          Nothing -> wrapGenState dp
  | otherwise = pure p

zapNTimes :: Int -> PartialGen a -> Gen (PartialGen a)
zapNTimes n
  | n <= 0 = pure
  | otherwise = zapNTimes (n - 1) <=< zap

evalPartial :: PartialGen a -> a
evalPartial (PartialGen (F m)) = m id continue

zapAntiGen :: Int -> AntiGen a -> Gen a
zapAntiGen n = fmap evalPartial <$> zapNTimes n <=< evalToPartial

runAntiGen :: AntiGen a -> Gen a
runAntiGen ag = evalPartial <$> evalToPartial ag
