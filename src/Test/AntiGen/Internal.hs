{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.AntiGen.Internal (
  AntiGen,
  QC (..),
  (|!),
  zapAntiGen,
  runAntiGen,
  evalToPartial,
  countDecisionPoints,
  hoistPartialGen,
) where

import Control.Applicative (liftA)
import Control.Monad ((<=<))
import Control.Monad.Free (Free (..))
import Control.Monad.Free.Church (F (..), MonadFree (..), fromF, iter, toF)
import Control.Monad.Identity (Identity (..))
import System.Random.Stateful (StatefulGen (..), UniformRange (..))
import Test.QuickCheck (Gen, QC (..))
import Test.QuickCheck.GenT (MonadGen (..))

data BiGen g m next where
  BiGen ::
    StatefulGen g m =>
    { bgPositiveGen :: g -> m t
    , bgNegativeGen :: Maybe (g -> m t)
    , bgContinuation :: t -> next
    } ->
    BiGen g m next

instance Functor (BiGen g m) where
  fmap f (BiGen p n c) = BiGen p n $ f . c

newtype AntiGen g m a = AntiGen (F (BiGen g m) a)
  deriving (Functor, Applicative, Monad)

mapGen ::
  forall g m a.
  StatefulGen g m =>
  (forall x. m x -> m x) ->
  AntiGen g m a ->
  AntiGen g m a
mapGen f (AntiGen (F m)) = runIdentity $ m (pure . pure) $ \BiGen {..} ->
  pure . wrap $
    BiGen (f . bgPositiveGen) (liftA f <$> bgNegativeGen) $
      runIdentity . bgContinuation

instance MonadGen (AntiGen QC Gen) where
  liftGen g = AntiGen $ F $ \p b -> b $ BiGen (const g) Nothing p
  variant n = mapGen (variant n)
  sized _f = undefined
  resize n = mapGen (resize n)
  choose = liftGen . choose

deriving newtype instance MonadFree (BiGen g m) (AntiGen g m)

(|!) :: Gen a -> Gen a -> AntiGen QC Gen a
pos |! neg = AntiGen $ F $ \p b ->
  b $ BiGen (const pos) (Just $ const neg) p

data DecisionPoint g m next where
  DecisionPoint ::
    StatefulGen g m =>
    { dpValue :: t
    , dpPositiveGen :: g -> m t
    , dpNegativeGen :: Maybe (g -> m t)
    , dpContinuation :: t -> next
    } ->
    DecisionPoint g m next

instance Functor (DecisionPoint g m) where
  fmap f (DecisionPoint v p n c) = DecisionPoint v p n $ f . c

instance Foldable (DecisionPoint g m) where
  foldMap f = f . continue

instance Traversable (DecisionPoint g m) where
  sequenceA DecisionPoint {..} = DecisionPoint dpValue dpPositiveGen dpNegativeGen . const <$> dpContinuation dpValue

continue :: DecisionPoint g m next -> next
continue DecisionPoint {..} = dpContinuation dpValue

newtype PartialGen g m a = PartialGen (F (DecisionPoint g m) a)
  deriving (Functor, Applicative, Monad)

deriving newtype instance MonadFree (DecisionPoint g m) (PartialGen g m)

evalToPartial ::
  ( StatefulGen g m
  , MonadFree (DecisionPoint g m) m
  ) =>
  AntiGen g m a -> g -> m (PartialGen g m a)
evalToPartial (AntiGen (F m)) g = m (pure . pure) $ \BiGen {..} -> do
  value <- bgPositiveGen g
  wrap $ DecisionPoint value bgPositiveGen bgNegativeGen bgContinuation

countDecisionPoints :: PartialGen g m a -> Int
countDecisionPoints (PartialGen (F m)) =
  runIdentity . m (const $ pure 0) $ \dp@DecisionPoint {..} ->
    pure . maybe id (const succ) dpNegativeGen =<< continue dp

hoistPartialGen :: PartialGen g m a -> PartialGen g m a
hoistPartialGen (PartialGen (F m)) = m pure $ \DecisionPoint {..} -> do
  wrap . DecisionPoint dpValue dpPositiveGen dpNegativeGen $ dpContinuation

regenerate ::
  MonadFree (DecisionPoint g m) m =>
  PartialGen g m a ->
  g ->
  m (PartialGen g m a)
regenerate (PartialGen (F m)) g = m (pure . pure) $ \DecisionPoint {..} -> do
  value <- dpPositiveGen g
  wrap $ DecisionPoint value dpPositiveGen dpNegativeGen dpContinuation

zapPartial ::
  forall g m a.
  ( MonadFree (DecisionPoint g m) m
  , StatefulGen g m
  ) =>
  PartialGen g m a -> g -> m (PartialGen g m a)
zapPartial pg@(PartialGen f) g = do
  let nPoints = countDecisionPoints pg
  breakAt <- uniformRM (0, nPoints - 1) g
  let
    go :: forall b. Int -> Free (DecisionPoint g m) b -> m (PartialGen g m b)
    go _ (Pure x) = pure $ pure x
    go 0 (Free DecisionPoint {..})
      | Just negativeGen <- dpNegativeGen = do
          value <- negativeGen g
          wrap $
            DecisionPoint value negativeGen Nothing $
              (`regenerate` g)
                <=< pure . PartialGen . toF . dpContinuation
    go d (Free DecisionPoint {..}) = wrap $
      DecisionPoint dpValue dpPositiveGen dpNegativeGen $
        \x -> go (maybe d (const $ pred d) dpNegativeGen) (dpContinuation x)
  go breakAt $ fromF f

evalPartialGen :: PartialGen g m a -> a
evalPartialGen (PartialGen m) = iter continue m

zapNTimes ::
  ( MonadFree (DecisionPoint g m) m
  , StatefulGen g m
  ) =>
  Int -> g -> PartialGen g m a -> m (PartialGen g m a)
zapNTimes n g x
  | n > 0 = zapNTimes (n - 1) g =<< zapPartial x g
  | otherwise = pure x

-- | Make a negative case generator which generates at most `n` mistakes
zapAntiGen ::
  ( StatefulGen g m
  , MonadFree (DecisionPoint g m) m
  ) =>
  Int -> AntiGen g m a -> g -> m a
zapAntiGen n x g = do
  partial <- evalToPartial x g
  zapped <- zapNTimes n g partial
  pure $ evalPartialGen zapped

-- | Make a positive example generator
runAntiGen ::
  ( StatefulGen g m
  , MonadFree (DecisionPoint g m) m
  ) =>
  AntiGen g m a -> g -> m a
runAntiGen = zapAntiGen 0

hoistGen :: StatefulGen g m' => (forall x. m x -> m' x) -> AntiGen g m a -> AntiGen g m' a
hoistGen f (AntiGen (F m)) = m pure $ \BiGen {..} ->
  wrap $ BiGen (f <$> bgPositiveGen) (fmap f <$> bgNegativeGen) bgContinuation

runAntiGenQC :: AntiGen QC Gen a -> Gen a
runAntiGenQC x = undefined
