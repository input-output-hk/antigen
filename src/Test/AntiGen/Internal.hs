{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.AntiGen.Internal (
  AntiGen,
  ZapResult (..),
  prettyZapResult,
  (|!),
  (#!),
  zapAntiGen,
  zapAntiGenResult,
  runAntiGen,
  evalToPartial,
  evalPartial,
  countDecisionPoints,
  zapAt,
  withAnnotation,
) where

import Control.Monad ((<=<))
import Control.Monad.Free (Free (..))
import Control.Monad.Free.Church (F (..), MonadFree (..), fromF)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck (getSize)
import Test.QuickCheck.Gen (Gen (..))
import Test.QuickCheck.GenT (MonadGen (..))

data BiGen next where
  BiGen :: Gen t -> Maybe (Gen t) -> (t -> next) -> BiGen next
  Annotate :: Text -> AntiGen t -> (t -> next) -> BiGen next

instance Functor BiGen where
  fmap f (BiGen p n c) = BiGen p n $ f . c
  fmap f (Annotate ann inner c) = Annotate ann inner $ f . c

newtype AntiGen a = AntiGen (F BiGen a)
  deriving (Functor, Applicative, Monad, MonadFree BiGen)

mapGen :: (forall x. Gen x -> Gen x) -> AntiGen a -> AntiGen a
mapGen f (AntiGen (F m)) = m pure $ \case
  BiGen pos neg c -> wrap $ BiGen (f pos) (f <$> neg) c
  Annotate ann inner c -> wrap $ Annotate ann (mapGen f inner) c

instance MonadGen AntiGen where
  liftGen g = AntiGen $ F $ \p b -> b $ BiGen g Nothing p
  variant n = mapGen (variant n)
  sized f = wrap $ BiGen (f <$> getSize) Nothing id
  resize n m = mapGen (resize n) m
  choose = liftGen . choose

mkAntiGen :: Gen a -> Gen a -> AntiGen a
mkAntiGen active alt =
  AntiGen $ F $ \p b -> b $ BiGen (p <$> active) (Just $ p <$> alt) id

-- | Create a negatable generator by providing a positive and a negative
-- generator
(|!) :: Gen a -> Gen a -> AntiGen a
(|!) = mkAntiGen

(#!) :: AntiGen a -> Text -> AntiGen a
(#!) = flip withAnnotation

-- | Wrap an AntiGen with an annotation
withAnnotation :: Text -> AntiGen a -> AntiGen a
withAnnotation ann inner = wrap $ Annotate ann inner pure

data DecisionPoint next where
  DecisionPoint ::
    { dpValue :: t
    , dpActiveGen :: Gen t
    , dpAlternativeGen :: Maybe (Gen t)
    , dpAnnotation :: [Text]
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
evalToPartial (AntiGen f) = evalToPartialWithPath [] (fromF f)

evalToPartialWithPath :: [Text] -> Free BiGen a -> Gen (PartialGen a)
evalToPartialWithPath _ (Pure x) = pure $ pure x
evalToPartialWithPath path (Free (Annotate ann (AntiGen inner) cont)) = MkGen $ \qcGen sz ->
  -- Evaluate inner with extended path, then continue with original path,
  -- using split generators so randomness is threaded correctly.
  let (qcGenInner, qcGenCont) = split qcGen
      innerPartial =
        unGen
          (evalToPartialWithPath (path <> [ann]) (fromF inner))
          qcGenInner
          sz
   in innerPartial >>= \t ->
        unGen (evalToPartialWithPath path (cont t)) qcGenCont sz
evalToPartialWithPath path (Free (BiGen activeGen altGen cont)) = MkGen $ \qcGen sz ->
  let (qcGenValue, qcGenCont) = split qcGen
      value = unGen activeGen qcGenValue sz
   in wrap $
        DecisionPoint
          { dpValue = value
          , dpActiveGen = activeGen
          , dpAlternativeGen = altGen
          , dpAnnotation = path
          , dpContinuation = \v ->
              unGen (evalToPartialWithPath path (cont v)) qcGenCont sz
          }

countDecisionPoints :: PartialGen a -> Int
countDecisionPoints (PartialGen (F m)) = m (const 0) $ \dp@DecisionPoint {..} ->
  case dpAlternativeGen of
    Just _ -> succ $ continue dp
    Nothing -> continue dp

data ZapResult a = ZapResult
  { zrValue :: a
  , zrAnnotation :: [NonEmpty Text]
  , zrZapped :: !Int
  }
  deriving (Functor)

instance Semigroup (ZapResult a) where
  ZapResult v a1 z1 <> ZapResult _ a2 z2 =
    ZapResult v (a1 <> a2) (z1 + z2)

-- | Pretty print the annotation paths from a ZapResult
prettyZapResult :: ZapResult a -> Text
prettyZapResult ZapResult {..} =
  T.unlines $
    [ "Zapped " <> T.pack (show zrZapped) <> " decision points"
    ]
      <> case zrAnnotation of
        [] -> []
        anns -> "Annotations:" : map prettyPath anns
  where
    prettyPath :: NonEmpty Text -> Text
    prettyPath path = "  - " <> T.intercalate "." (NE.toList path)

zapAt :: Int -> PartialGen a -> Gen (ZapResult (PartialGen a))
zapAt cutoffDepth p@(PartialGen f)
  | countDecisionPoints p == 0 = pure $ ZapResult p [] 0
  | otherwise = MkGen $ \qcGen sz ->
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
                        , zrAnnotation = maybe [] (: []) (NE.nonEmpty dpAnnotation)
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
zap p
  | countDecisionPoints p == 0 = pure $ ZapResult p [] 0
  | otherwise = (`zapAt` p) =<< choose (0, countDecisionPoints p - 1)

zapNTimes :: Int -> PartialGen a -> Gen (ZapResult a)
zapNTimes n x
  | n <= 0 = pure $ ZapResult (evalPartial x) [] 0
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
