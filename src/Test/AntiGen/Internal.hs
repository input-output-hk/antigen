{-# LANGUAGE CPP #-}
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
  scaleWeight,
  reweigh,
) where

import Control.Monad ((<=<))
import Control.Monad.Free.Church (F (..), MonadFree (..))
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
#if MIN_VERSION_QuickCheck(2,18,0)
import System.Random (SplitGen (..))
#else
import System.Random (RandomGen (split))
#endif
import Test.QuickCheck (getSize)
import Test.QuickCheck.Gen (Gen (..))
import Test.QuickCheck.GenT (MonadGen (..))
import Test.QuickCheck.Random (QCGen)

#if !MIN_VERSION_QuickCheck(2,18,0)
splitGen :: RandomGen g => g -> (g, g)
splitGen = split
#endif

data BiGen next where
  BiGen :: Gen t -> Maybe (Gen t) -> Float -> (t -> next) -> BiGen next
  Annotate :: Text -> AntiGen t -> (t -> next) -> BiGen next
  Reweigh :: (Float -> Float) -> AntiGen t -> (t -> next) -> BiGen next

instance Functor BiGen where
  fmap f (BiGen p n w c) = BiGen p n w $ f . c
  fmap f (Annotate ann inner c) = Annotate ann inner $ f . c
  fmap f (Reweigh g inner c) = Reweigh g inner $ f . c

newtype AntiGen a = AntiGen (F BiGen a)
  deriving (Functor, Applicative, Monad, MonadFree BiGen)

mapGen :: (forall x. Gen x -> Gen x) -> AntiGen a -> AntiGen a
mapGen f (AntiGen (F m)) = m pure $ \case
  BiGen pos neg w c -> wrap $ BiGen (f pos) (f <$> neg) w c
  Annotate ann inner c -> wrap $ Annotate ann (mapGen f inner) c
  Reweigh g inner c -> wrap $ Reweigh g (mapGen f inner) c

instance MonadGen AntiGen where
  liftGen g = AntiGen $ F $ \p b -> b $ BiGen g Nothing 1 p
  variant n = mapGen (variant n)
  sized f = wrap $ BiGen (f <$> getSize) Nothing 1 id
  resize n m = mapGen (resize n) m
  choose = liftGen . choose

mkAntiGen :: Gen a -> Gen a -> AntiGen a
mkAntiGen active alt =
  AntiGen $ F $ \p b -> b $ BiGen (p <$> active) (Just $ p <$> alt) 1 id

-- | Create a negatable generator by providing a positive and a negative
-- generator
(|!) :: Gen a -> Gen a -> AntiGen a
(|!) = mkAntiGen

infixl 6 |!

-- | Postfix annotation operator. Annotates an 'AntiGen' with a label that
-- will be included in 'ZapResult' when this generator is zapped.
--
-- @
-- myGen = positive |! negative #! "sign"
-- @
(#!) :: AntiGen a -> Text -> AntiGen a
(#!) = flip withAnnotation

infixl 5 #!

-- | Wrap an AntiGen with an annotation
withAnnotation :: Text -> AntiGen a -> AntiGen a
withAnnotation ann inner = wrap $ Annotate ann inner pure

scaleWeight :: (Float -> Float) -> AntiGen a -> AntiGen a
scaleWeight rw inner = wrap $ Reweigh rw inner pure

reweigh :: Float -> AntiGen a -> AntiGen a
reweigh w = scaleWeight $ const w

data DecisionPoint next where
  DecisionPoint ::
    { dpValue :: t
    , dpActiveGen :: Gen t
    , dpAlternativeGen :: Maybe (Gen t)
    , dpAnnotation :: Seq Text
    , dpWeight :: Float
    , dpContinuation :: t -> next
    } ->
    DecisionPoint next

instance Functor DecisionPoint where
  fmap f (DecisionPoint v p n a w c) = DecisionPoint v p n a w $ f . c

continue :: DecisionPoint next -> next
continue DecisionPoint {..} = dpContinuation dpValue

newtype PartialGen a = PartialGen (F DecisionPoint a)
  deriving (Functor, Applicative, Monad, MonadFree DecisionPoint)

evalToPartial :: AntiGen a -> Gen (PartialGen a)
evalToPartial (AntiGen (F m)) = MkGen $ \qcGen sz ->
  m kp kf Seq.empty id qcGen sz
  where
    kp :: a -> Seq Text -> (Float -> Float) -> QCGen -> Int -> PartialGen a
    kp x _ _ _ _ = pure x

    kf ::
      BiGen (Seq Text -> (Float -> Float) -> QCGen -> Int -> PartialGen a) ->
      Seq Text ->
      (Float -> Float) ->
      QCGen ->
      Int ->
      PartialGen a
    kf (BiGen activeGen altGen w cont) path rw qcGen sz =
      let (qcGenValue, qcGenCont) = splitGen qcGen
          value = unGen activeGen qcGenValue sz
       in wrap $
            DecisionPoint
              { dpValue = value
              , dpActiveGen = activeGen
              , dpAlternativeGen = altGen
              , dpAnnotation = path
              , dpWeight = rw w
              , dpContinuation = \v -> cont v path rw qcGenCont sz
              }
    kf (Annotate ann (AntiGen (F inner)) cont) path rw qcGen sz = do
      let (qcGenInner, qcGenCont) = splitGen qcGen
      t <- inner kp kf (path :|> ann) rw qcGenInner sz
      cont t path rw qcGenCont sz
    kf (Reweigh g (AntiGen (F inner)) cont) path rw qcGen sz = do
      let (qcGenInner, qcGenCont) = splitGen qcGen
      t <- inner kp kf path (g . rw) qcGenInner sz
      cont t path rw qcGenCont sz

countDecisionPoints :: PartialGen a -> Int
countDecisionPoints (PartialGen (F m)) = m (const 0) $ \dp@DecisionPoint {..} ->
  case dpAlternativeGen of
    Just _ -> succ $ continue dp
    Nothing -> continue dp

totalWeight :: PartialGen a -> Float
totalWeight (PartialGen (F m)) = m (const 0) $ \dp@DecisionPoint {..} ->
  case dpAlternativeGen of
    Just _ -> continue dp + dpWeight
    Nothing -> continue dp

data ZapResult a = ZapResult
  { zrValue :: a
  , zrAnnotation :: [NonEmpty Text]
  , zrZapped :: Int
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

zapAt :: Float -> PartialGen a -> Gen (ZapResult (PartialGen a))
zapAt cutoffWeight (PartialGen (F m)) = MkGen $ \qcGen sz ->
  m kp (kf qcGen sz) $ Just cutoffWeight
  where
    kp :: a -> Maybe Float -> ZapResult (PartialGen a)
    kp x _ = ZapResult (pure x) mempty 0

    kf ::
      QCGen ->
      Int ->
      DecisionPoint (Maybe Float -> ZapResult (PartialGen a)) ->
      Maybe Float ->
      ZapResult (PartialGen a)
    kf qcGen sz DecisionPoint {..} mn =
      case dpAlternativeGen of
        Just altGen
          | Just n <- mn
          , n < dpWeight ->
              ZapResult
                { zrValue =
                    let newValue = unGen altGen qcGen sz
                     in wrap $
                          DecisionPoint
                            { dpValue = newValue
                            , dpActiveGen = altGen
                            , dpAlternativeGen = Nothing
                            , dpContinuation = \v -> zrValue (dpContinuation v Nothing)
                            , ..
                            }
                , zrAnnotation = toList (NE.nonEmpty (toList dpAnnotation))
                , zrZapped = 1
                }
        _ ->
          -- Preserve tree structure
          let n' = case dpAlternativeGen of
                Just _ -> (\x -> x - dpWeight) <$> mn
                Nothing -> mn
              restResult = dpContinuation dpValue n'
           in ZapResult
                { zrValue =
                    wrap $
                      DecisionPoint
                        { dpContinuation = \v -> zrValue (dpContinuation v n')
                        , ..
                        }
                , zrAnnotation = zrAnnotation restResult
                , zrZapped = zrZapped restResult
                }

zap :: PartialGen a -> Gen (ZapResult (PartialGen a))
zap p =
  let w = totalWeight p
   in if w == 0
        then pure $ ZapResult p [] 0
        else (`zapAt` p) =<< choose (0, w)

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
zapAntiGen n = fmap zrValue . zapAntiGenResult n

-- | Create a negative generator from an `AntiGen` by introducing at most
-- `n` mistakes.
zapAntiGenResult :: Int -> AntiGen a -> Gen (ZapResult a)
zapAntiGenResult n = zapNTimes n <=< evalToPartial

-- | Create a positive generator from the provided `AntiGen`.
runAntiGen :: AntiGen a -> Gen a
runAntiGen ag = evalPartial <$> evalToPartial ag
