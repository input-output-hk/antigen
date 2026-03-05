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

data DecisionPoint next where
  DecisionPoint ::
    { dpValue :: t
    , dpActiveGen :: Gen t
    , dpAlternativeGen :: Maybe (Gen t)
    , dpAnnotation :: Seq Text
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
  m kp kf Seq.empty qcGen sz
  where
    kp :: a -> Seq Text -> QCGen -> Int -> PartialGen a
    kp x _ _ _ = pure x

    kf ::
      BiGen (Seq Text -> QCGen -> Int -> PartialGen a) ->
      Seq Text ->
      QCGen ->
      Int ->
      PartialGen a
    kf (BiGen activeGen altGen cont) path qcGen sz =
      let (qcGenValue, qcGenCont) = splitGen qcGen
          value = unGen activeGen qcGenValue sz
       in wrap $
            DecisionPoint
              { dpValue = value
              , dpActiveGen = activeGen
              , dpAlternativeGen = altGen
              , dpAnnotation = path
              , dpContinuation = \v -> cont v path qcGenCont sz
              }
    kf (Annotate ann (AntiGen (F inner)) cont) path qcGen sz = do
      let (qcGenInner, qcGenCont) = splitGen qcGen
      t <- inner kp kf (path :|> ann) qcGenInner sz
      cont t path qcGenCont sz

countDecisionPoints :: PartialGen a -> Int
countDecisionPoints (PartialGen (F m)) = m (const 0) $ \dp@DecisionPoint {..} ->
  case dpAlternativeGen of
    Just _ -> succ $ continue dp
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

zapAt :: Int -> PartialGen a -> Gen (ZapResult (PartialGen a))
zapAt cutoffDepth (PartialGen (F m)) = MkGen $ \qcGen sz ->
  m kp (kf qcGen sz) cutoffDepth
  where
    kp :: a -> Int -> ZapResult (PartialGen a)
    kp x _ = ZapResult (pure x) mempty 0

    kf ::
      QCGen ->
      Int ->
      DecisionPoint (Int -> ZapResult (PartialGen a)) ->
      Int ->
      ZapResult (PartialGen a)
    kf qcGen sz DecisionPoint {..} n =
      case dpAlternativeGen of
        Just altGen
          | n == 0 ->
              -- Zap here, then go negative
              ZapResult
                { zrValue =
                    let newValue = unGen altGen qcGen sz
                     in wrap $
                          DecisionPoint
                            { dpValue = newValue
                            , dpActiveGen = altGen
                            , dpAlternativeGen = Nothing
                            , dpContinuation = \v -> zrValue (dpContinuation v (-1))
                            , ..
                            }
                , zrAnnotation = toList (NE.nonEmpty (toList dpAnnotation))
                , zrZapped = 1
                }
        _ ->
          -- Preserve tree structure
          let n' = case dpAlternativeGen of
                Just _ -> pred n
                Nothing -> n
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
  let n = countDecisionPoints p
   in if n == 0
        then pure $ ZapResult p [] 0
        else (`zapAt` p) =<< choose (0, n - 1)

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
