{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Test.AntiGen.Internal (
  AntiGen,
  ZapResult (..),
  (|!),
  zapAntiGen,
  zapAntiGenResult,
  runAntiGen,
  evalToPartial,
  evalPartial,
  countDecisionPoints,
  zapAt,
  annotatedAnti,
) where

import Control.Monad ((<=<))
import Control.Monad.Free (Free (..))
import Control.Monad.Free.Church (F (..), MonadFree (..), fromF)
import Data.Text (Text)
import Test.QuickCheck (getSize)
import Test.QuickCheck.Gen (Gen (..))
import Test.QuickCheck.GenT (MonadGen (..))

data BiGen next where
  BiGen ::
    { bgActiveGen :: Gen t
    , bgAlternativeGen :: Maybe (Gen t)
    , bgAnnotation :: Maybe Text
    , bgContinuation :: t -> next
    } ->
    BiGen next

instance Functor BiGen where
  fmap f (BiGen p n a c) = BiGen p n a $ f . c

newtype AntiGen a = AntiGen (F BiGen a)
  deriving (Functor, Applicative, Monad, MonadFree BiGen)

mapGen :: (forall x. Gen x -> Gen x) -> AntiGen a -> AntiGen a
mapGen f (AntiGen (F m)) = m pure $ \(BiGen pos neg a c) ->
  wrap $ BiGen (f pos) (f <$> neg) a c

instance MonadGen AntiGen where
  liftGen g = AntiGen $ F $ \p b -> b $ BiGen g Nothing mempty p
  variant n = mapGen (variant n)
  sized f = wrap $ BiGen (f <$> getSize) Nothing mempty id
  resize n m = mapGen (resize n) m
  choose = liftGen . choose

mkAntiGen :: Maybe Text -> Gen a -> Gen a -> AntiGen a
mkAntiGen ann active alt =
  AntiGen $ F $ \p b -> b $ BiGen (p <$> active) (Just $ p <$> alt) ann id

-- | Create a negatable generator by providing a positive and a negative
-- generator
(|!) :: Gen a -> Gen a -> AntiGen a
(|!) = mkAntiGen Nothing

annotatedAnti :: Text -> Gen a -> Gen a -> AntiGen a
annotatedAnti annotation = mkAntiGen $ Just annotation

data DecisionPoint next where
  DecisionPoint ::
    { dpValue :: t
    , dpActiveGen :: Gen t
    , dpAlternativeGen :: Maybe (Gen t)
    , dpAnnotation :: Maybe Text
    , dpContinuation :: t -> next
    } ->
    DecisionPoint next

instance Functor DecisionPoint where
  fmap f (DecisionPoint v p n a c) = DecisionPoint v p n a $ f . c

continue :: DecisionPoint next -> next
continue DecisionPoint {..} = dpContinuation dpValue

newtype PartialGen a = PartialGen (F DecisionPoint a)
  deriving (Functor, Applicative, Monad, MonadFree DecisionPoint)

evalToPartial :: AntiGen a -> Gen (PartialGen a)
evalToPartial (AntiGen (F m)) = MkGen $ \qcGen sz ->
  m pure $ \BiGen {..} ->
    wrap $
      DecisionPoint
        { dpValue = unGen bgActiveGen qcGen sz
        , dpActiveGen = bgActiveGen
        , dpAlternativeGen = bgAlternativeGen
        , dpAnnotation = bgAnnotation
        , dpContinuation = bgContinuation
        }

countDecisionPoints :: PartialGen a -> Int
countDecisionPoints (PartialGen (F m)) = m (const 0) $ \dp@DecisionPoint {..} ->
  case dpAlternativeGen of
    Just _ -> succ $ continue dp
    Nothing -> continue dp

data ZapResult a = ZapResult
  { zrValue :: a
  , zrAnnotation :: [Text]
  , zrZapped :: Int
  }
  deriving (Functor)

instance Semigroup (ZapResult a) where
  ZapResult v a1 z1 <> ZapResult _ a2 z2 =
    ZapResult v (a1 <> a2) (z1 + z2)

zapAt :: Int -> PartialGen a -> Gen (ZapResult (PartialGen a))
zapAt cutoffDepth (PartialGen f) =
  MkGen $ \qcGen sz ->
    let
      go :: Int -> Free DecisionPoint a -> ZapResult (PartialGen a)
      go n = \case
        Pure x -> ZapResult (pure x) mempty 0
        Free dp@DecisionPoint {..} ->
          case dpAlternativeGen of
            Nothing ->
              let ZapResult _ ann zapped = go n $ continue dp
               in ZapResult
                    { zrValue =
                        wrap $
                          DecisionPoint
                            { dpContinuation = zrValue . go n . dpContinuation
                            , ..
                            }
                    , zrAnnotation = ann
                    , zrZapped = zapped
                    }
            Just altGen
              | n == 0 ->
                  let ZapResult _ _ zapped = go (pred n) $ continue dp
                   in ZapResult
                        { zrValue =
                            let newValue = unGen altGen qcGen sz
                             in wrap $
                                  DecisionPoint
                                    { dpValue = newValue
                                    , dpActiveGen = altGen
                                    , dpAlternativeGen = Nothing
                                    , dpContinuation = zrValue . go (pred n) . dpContinuation
                                    , ..
                                    }
                        , zrAnnotation = foldMap (: []) dpAnnotation
                        , zrZapped = succ zapped
                        }
              | otherwise ->
                  let ZapResult _ ann zapped = go (pred n) $ continue dp
                   in ZapResult
                        { zrValue =
                            wrap $
                              DecisionPoint
                                { dpContinuation = zrValue . go (pred n) . dpContinuation
                                , ..
                                }
                        , zrAnnotation = ann
                        , zrZapped = zapped
                        }
     in
      go cutoffDepth $ fromF f

zap :: PartialGen a -> Gen (ZapResult (PartialGen a))
zap p = (`zapAt` p) =<< choose (0, countDecisionPoints p - 1)

zapNTimes :: Int -> PartialGen a -> Gen (ZapResult a)
zapNTimes n x
  | n <= 0 = pure $ ZapResult (evalPartial x) mempty 0
  | otherwise = do
      zapResult <- zap x
      rest <- zapNTimes (pred n) $ zrValue zapResult
      pure $ rest <> fmap evalPartial zapResult

evalPartial :: PartialGen a -> a
evalPartial (PartialGen (F m)) = m id continue

-- | Create a negative generator from an `AntiGen` by introducing at most
-- `n` mistakes. If there are no negatable generators in the `AntiGen`, it will
-- return a positive generator. Also if the number of negatable generators in
-- the `AntiGen` is lower than `n`, then the number of negations will be less
-- than `n`.
zapAntiGen :: Int -> AntiGen a -> Gen a
zapAntiGen = fmap (fmap zrValue) . zapAntiGenResult

-- | Create a negative generator from an `AntiGen` by introducing at most
-- `n` mistakes. If there are no decision points, it will return `Nothing`.
zapAntiGenResult :: Int -> AntiGen a -> Gen (ZapResult a)
zapAntiGenResult n = zapNTimes n <=< evalToPartial

-- | Create a positive generator from the provided `AntiGen`.
runAntiGen :: AntiGen a -> Gen a
runAntiGen ag = evalPartial <$> evalToPartial ag
