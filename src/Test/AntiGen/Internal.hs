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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.AntiGen.Internal (
  AntiGen,
  AntiGenT,
  QC (..),
  (|!),
  zapAntiGen,
  runAntiGen,
  evalToPartial,
  countDecisionPoints,
  hoistPartialGen,
) where

import Control.Applicative (liftA)
import Control.Monad.Free.Church (MonadFree (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.State.Strict (MonadState (..), evalStateT, modify)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Free.Church (FT (..))
import System.Random.Stateful (StatefulGen (..), UniformRange (..))
import Test.QuickCheck (Gen, QC (..))
import Test.QuickCheck.Gen (Gen (..))
import Test.QuickCheck.GenT (GenT (..), MonadGen (..), runGenT)

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

newtype AntiGenT g m m' a = AntiGenT (FT (BiGen g m) m' a)
  deriving (Functor, Applicative, Monad)

type AntiGen = AntiGenT QC Gen Identity

mapGen ::
  forall g m a.
  StatefulGen g m =>
  (forall x. m x -> m x) ->
  AntiGenT g m Identity a ->
  AntiGenT g m Identity a
mapGen f (AntiGenT (FT m)) = runIdentity $ m (pure . pure) $ \ret BiGen {..} ->
  pure . wrap $
    BiGen (f . bgPositiveGen) (liftA f <$> bgNegativeGen) $
      runIdentity . ret . bgContinuation

instance MonadGen (AntiGenT QC Gen Identity) where
  liftGen g = AntiGenT $ FT $ \p b -> b id $ BiGen (const g) Nothing p
  variant n = mapGen (variant n)
  sized _f = undefined
  resize n = mapGen (resize n)
  choose = liftGen . choose

deriving newtype instance MonadTrans (AntiGenT g m)

deriving newtype instance MonadFree (BiGen g m) (AntiGenT g m m')

(|!) :: Applicative m' => Gen a -> Gen a -> AntiGenT QC Gen m' a
pos |! neg = AntiGenT $ FT $ \p b ->
  b p $ BiGen (const pos) (Just $ const neg) id

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

continue :: DecisionPoint g m next -> next
continue DecisionPoint {..} = dpContinuation dpValue

newtype PartialGenT g m m' a = PartialGenT (FT (DecisionPoint g m) m' a)
  deriving (Functor, Applicative, Monad)

deriving newtype instance MonadFree (DecisionPoint g m) (PartialGenT g m m')

deriving newtype instance MonadTrans (PartialGenT g m)

evalToPartial ::
  StatefulGen g m =>
  AntiGenT g m Identity a -> g -> PartialGenT g m m a
evalToPartial (AntiGenT (FT m)) g = runIdentity $ m (pure . pure) $ \r BiGen {..} -> pure $ do
  value <- lift $ bgPositiveGen g
  wrap . DecisionPoint value bgPositiveGen bgNegativeGen $ runIdentity . r . bgContinuation

countDecisionPoints :: Monad m' => PartialGenT g m m' a -> m' Int
countDecisionPoints (PartialGenT (FT m)) =
  m (const $ pure 0) $ \r dp@DecisionPoint {..} ->
    pure . maybe id (const succ) dpNegativeGen =<< r (continue dp)

hoistPartialGen :: Applicative m' => PartialGenT g m Identity a -> PartialGenT g m m' a
hoistPartialGen (PartialGenT (FT m)) = runIdentity . m (pure . pure) $ \r DecisionPoint {..} -> do
  pure . wrap . DecisionPoint dpValue dpPositiveGen dpNegativeGen $ runIdentity . r . dpContinuation

runPartialG :: PartialGenT QC Gen Gen a -> Gen (PartialGenT QC Gen Identity a)
runPartialG (PartialGenT (FT m)) = m (pure . pure) $ \r DecisionPoint {..} -> runGenT . GenT $ \g sz -> do
  wrap . DecisionPoint dpValue dpPositiveGen dpNegativeGen $ \x -> unGen (r $ dpContinuation x) g sz

zap ::
  StatefulGen g m =>
  PartialGenT g m Identity a -> g -> PartialGenT g m m a
zap p@(PartialGenT (FT m)) g
  | let maxDepth = runIdentity $ countDecisionPoints p
  , maxDepth > 0 = do
      cutoffDepth <- lift $ uniformRM (0, maxDepth - 1) g
      (`evalStateT` cutoffDepth) <$> runIdentity . m (pure . pure) $ \r dp@DecisionPoint {..} ->
        let cont = wrap $ runIdentity . r <$> dp
         in case dpNegativeGen of
              Just negativeGen -> pure $ do
                d <- get
                modify pred
                case compare 0 d of
                  LT -> cont
                  EQ -> do
                    value <- lift . lift $ negativeGen g
                    wrap . DecisionPoint value negativeGen Nothing $ runIdentity . r . dpContinuation
                  GT -> do
                    value <- lift . lift $ dpPositiveGen g
                    wrap . DecisionPoint value dpPositiveGen dpNegativeGen $ runIdentity . r . dpContinuation
              Nothing -> pure cont
  | otherwise = hoistPartialGen p

zapNTimes :: Int -> PartialGenT QC Gen Identity a -> Gen (PartialGenT QC Gen Identity a)
zapNTimes n x
  | n > 0 = zapNTimes (n - 1) =<< runPartialG (zap x QC)
  | otherwise = pure x

evalPartial :: Monad m' => PartialGenT g m m' a -> m' a
evalPartial (PartialGenT (FT m)) = m pure $ \r dp -> r $ continue dp

-- | Make a negative case generator which generates at most `n` mistakes
zapAntiGen :: Int -> AntiGenT QC Gen Identity a -> Gen a
zapAntiGen n x = do
  partial <- runPartialG $ evalToPartial x QC
  zapped <- zapNTimes n partial
  pure . runIdentity $ evalPartial zapped

-- | Make a positive example generator
runAntiGen :: AntiGenT QC Gen Identity a -> Gen a
runAntiGen = zapAntiGen 0
