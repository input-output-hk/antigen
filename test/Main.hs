{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad (replicateM)
import Data.Data (Proxy (..))
import Test.AntiGen (
  AntiGen,
  antiNegative,
  antiNonNegative,
  antiNonPositive,
  antiPositive,
  faultyBool,
  faultyNum,
  faultyTry,
  runAntiGen,
  zapAntiGen,
  (|!),
  (||!),
 )
import Test.AntiGen.Internal (countDecisionPoints, evalToPartial)
import Test.Hspec (Spec, describe, hspec, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (
  Arbitrary (..),
  CoArbitrary,
  Gen,
  NonNegative (..),
  NonPositive (..),
  Positive (..),
  Property,
  Testable (..),
  counterexample,
  forAll,
  forAllBlind,
  getSize,
  label,
  scale,
  suchThat,
  vector,
  (.&&.),
  (.||.),
  (=/=),
  (===),
 )
import Test.QuickCheck.GenT (MonadGen (..), listOf1, oneof)

antiGenPositive :: AntiGen Int
antiGenPositive = (getPositive @Int <$> arbitrary) |! (getNonPositive <$> arbitrary)

antiGenTuple :: AntiGen (Int, Int)
antiGenTuple = do
  x <- antiGenPositive
  y <- antiGenPositive
  pure (x, y)

antiGenSmall :: AntiGen Int
antiGenSmall = choose (0, 5) |! choose (6, 10)

antiGenLengthStringStatic :: AntiGen (Int, String)
antiGenLengthStringStatic = do
  l <- antiGenSmall
  pure (l, replicate l 'a')

antiGenLengthString :: AntiGen (Int, String)
antiGenLengthString = do
  l <- antiGenSmall
  s <-
    pure (replicate l 'a') |! do
      NonNegative l' <- suchThat arbitrary $ \(NonNegative x) -> x /= l
      pure $ replicate l' 'b'
  pure (l, s)

antiGenEither :: AntiGen (Either Int [Bool])
antiGenEither = do
  oneof
    [ Left <$> antiGenPositive
    , Right <$> do
        l <- antiGenSmall
        replicateM l $ pure True |! pure False
    ]

noneOf :: [Bool] -> Property
noneOf [] = property True
noneOf (x : xs) = not x .&&. noneOf xs

exactlyOne :: [(String, Bool)] -> Property
exactlyOne [] = counterexample "None of the conditions hold" $ property False
exactlyOne ((lbl, p) : ps) = label lbl (p .&&. noneOf (snd <$> ps)) .||. (not p .&&. exactlyOne ps)

someGen :: (Arbitrary a, CoArbitrary a) => Proxy a -> Gen (Gen a)
someGen p =
  oneof
    [ pure <$> arbitrary
    , do
        x <- scale (`div` 2) $ someGen p
        f <- arbitrary
        pure $ f <$> x
    , do
        x <- scale (`div` 4) $ someGen p
        y <- scale (`div` 4) $ someGen p
        f <- arbitrary
        pure $ f <$> x <*> y
    ]

zapAntiGenSpec :: Spec
zapAntiGenSpec =
  describe "zapAntiGen" $ do
    prop "zapping `antiGenPositive` once generates negative examples" $ do
      x <- zapAntiGen 1 antiGenPositive
      pure $ x <= 0
    prop "zapping `antiGenPositive` zero times generates a positive example" $ do
      x <- zapAntiGen 0 antiGenPositive
      pure $ x > 0
    prop "zapping `antiGenTuple` once results in a single non-positive Int" $ do
      (x, y) <- zapAntiGen 1 antiGenTuple
      pure $
        label "x is non-positive" (x <= 0) .||. label "y is non-positive" (y <= 0)
    prop "zapping `antiGenTuple` twice results in two non-positive Ints" $ do
      (x, y) <- zapAntiGen 2 antiGenTuple
      pure $
        counterexample ("x = " <> show x <> " is positive") (x <= 0)
          .&&. counterexample ("y = " <> show y <> " is positive") (y <= 0)
    prop
      "zapping the length of the string propagates to the string generator"
      . forAll (zapAntiGen 1 antiGenLengthStringStatic)
      $ \(l, s) -> length s === l
    prop
      "zapping `antiGenLengthString` either generates invalid Int or a string of invalid length"
      . forAll (zapAntiGen 1 antiGenLengthString)
      $ \(l, s) ->
        exactlyOne
          [ ("l > 5", l > 5)
          , ("length s /= l", length s /= l)
          ]
    prop
      "zapping `antiGenEither` once gives a nice distribution"
      . forAll (zapAntiGen 1 antiGenEither)
      $ \x ->
        exactlyOne
          [
            ( "Left v <= 0"
            , case x of
                Right _ -> False
                Left v -> v <= 0
            )
          ,
            ( "Right length (filter not v) == 1"
            , case x of
                Left _ -> False
                Right v -> length (filter not v) == 1
            )
          ,
            ( "Right length > 5"
            , case x of
                Left _ -> False
                Right v -> length v > 5
            )
          ]

utilsSpec :: Spec
utilsSpec =
  describe "utils" $ do
    describe "faultyNum" $ do
      prop "positive" $ \(n :: Int) -> do
        res <- runAntiGen $ faultyNum n
        pure $ res === n
      prop "negative" $ \(n :: Int) -> do
        res <- zapAntiGen 1 $ faultyNum n
        pure $ res =/= n
    describe "faultyBool" $ do
      prop "positive" $ \b -> do
        res <- runAntiGen $ faultyBool b
        pure $ res === b
      prop "negative" $ \b -> do
        res <- zapAntiGen 1 $ faultyBool b
        pure $ res =/= b
    describe "faultyTry" $ do
      describe "String" $ do
        prop "positive" $ \(s :: String) -> do
          res <- runAntiGen $ faultyTry s
          pure $ res === s
        prop "negative" $ \(s :: String) -> do
          res <- zapAntiGen 1 $ faultyTry s
          pure $ res =/= s
    describe "antiPositive" $ do
      prop "positive" . forAll (runAntiGen $ antiPositive @Int) $ (> 0)
      prop "negative" . forAll (zapAntiGen 1 $ antiPositive @Int) $ (<= 0)
    describe "antiNegative" $ do
      prop "positive" . forAll (runAntiGen $ antiNegative @Int) $ (< 0)
      prop "negative" . forAll (zapAntiGen 1 $ antiNegative @Int) $ (>= 0)
    describe "antiNonPositive" $ do
      prop "positive" . forAll (runAntiGen $ antiNonPositive @Int) $ (<= 0)
      prop "negative" . forAll (zapAntiGen 1 $ antiNonPositive @Int) $ (> 0)
    describe "antiNonNegative" $ do
      prop "positive" . forAll (runAntiGen $ antiNonNegative @Int) $ (>= 0)
      prop "negative" . forAll (zapAntiGen 1 $ antiNonNegative @Int) $ (< 0)
    describe "(||!)" $ do
      prop "positive" $ do
        res <- runAntiGen $ listOf1 (antiPositive @Int) ||! pure []
        pure $
          counterexample "is empty" (not $ null res)
            .&&. counterexample "non-positive" (null $ filter (<= 0) res)
      prop "negative" $ do
        res <- zapAntiGen 1 $ listOf1 (antiPositive @Int) ||! pure []
        pure $
          exactlyOne
            [ ("null", null res)
            , ("nonpositive", length (filter (<= 0) res) == 1)
            ]

main :: IO ()
main = hspec $ do
  describe "AntiGen" $ do
    describe "treeDepth" $ do
      prop "pure has depth of zero" $ do
        pt <- evalToPartial $ pure ()
        pure $ countDecisionPoints pt `shouldBe` 0
      prop "single bind has depth of one, right identity holds" $ do
        let
          m = return =<< antiGenPositive
        pt <- evalToPartial m
        pt' <- evalToPartial antiGenPositive
        pure $ countDecisionPoints pt === countDecisionPoints pt' .&&. countDecisionPoints pt === 1
    zapAntiGenSpec
    describe "runAntiGen" $ do
      prop "runAntiGen . liftGen == id" $
        \(seed :: Int) -> forAllBlind (someGen $ Proxy @Int) $ \g -> do
          let g' = runAntiGen (liftGen g)
          res <- variant seed g
          res' <- variant seed g'
          pure $ res === res'
    describe "MonadGen" $ do
      prop "applying `sized` to a negatable generator preserves negation" $ do
        size <- getSize
        val <- zapAntiGen 1 . sized $ \sz -> pure sz |! pure (-sz)
        pure $ val === -size
      prop "`resize` has an effect when zapping" $ do
        sz <- choose (0, 10)
        val :: [Bool] <- zapAntiGen 1 . resize sz . sized $ \s ->
          liftGen (vector $ 2 * s) |! liftGen (vector s)
        pure $ length val === sz
      prop "nested `resize` works correctly" $ do
        x <- resize 30 $ do
          a <- getSize
          b <- scale (+ 1) $ do
            c <- getSize
            d <- scale (+ 1) getSize
            pure [c, d]
          pure $ a : b
        pure $ x === [30, 31, 32]
    utilsSpec
