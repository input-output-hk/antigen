{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad (replicateM)
import Test.AntiGen (AntiGen, always, countDecisionPoints, evalToPartial, runAntiGen, sometimes)
import Test.Hspec (describe, hspec, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (
  Arbitrary (..),
  NonPositive (..),
  Positive (..),
  Property,
  Testable (..),
  choose,
  counterexample,
  forAll,
  label,
  suchThat,
  (.&&.),
  (.||.),
  (===), NonNegative (..),
 )

antiGenPositive :: AntiGen Int
antiGenPositive = (getPositive @Int <$> arbitrary) `sometimes` (getNonPositive <$> arbitrary)

antiGenTuple :: AntiGen (Int, Int)
antiGenTuple = do
  x <- antiGenPositive
  y <- antiGenPositive
  pure (x, y)

antiGenSmall :: AntiGen Int
antiGenSmall = choose (0, 5) `sometimes` choose (6, 10)

antiGenLengthStringStatic :: AntiGen (Int, String)
antiGenLengthStringStatic = do
  l <- antiGenSmall
  pure (l, replicate l 'a')

antiGenLengthString :: AntiGen (Int, String)
antiGenLengthString = do
  l <- antiGenSmall
  s <-
    pure (replicate l 'a') `sometimes` do
      NonNegative l' <- suchThat arbitrary $ \(NonNegative x) -> x /= l
      pure $ replicate l' 'b'
  pure (l, s)

antiGenEither :: AntiGen (Either Int [Bool])
antiGenEither = do
  genLeft <- always arbitrary
  if genLeft
    then Left <$> antiGenPositive
    else
      Right <$> do
        l <- antiGenSmall
        replicateM l $ pure True `sometimes` pure False

noneOf :: [Bool] -> Property
noneOf [] = property True
noneOf (x : xs) = not x .&&. noneOf xs

exactlyOne :: [Bool] -> Property
exactlyOne [] = counterexample "None of the conditions hold" $ property False
exactlyOne (p : ps) = property $ (p .&&. noneOf ps) .||. (not p .&&. exactlyOne ps)

main :: IO ()
main = hspec $ do
  describe "AntiGen" $ do
    describe "treeDepth" $ do
      prop "pure has depth of zero" $ do
        let
          m = pure ()
        pt <- evalToPartial m
        pure $ countDecisionPoints pt `shouldBe` 0
      prop "single bind has depth of one, right identity holds" $ do
        let
          m = return =<< antiGenPositive
        pt <- evalToPartial m
        pt' <- evalToPartial antiGenPositive
        pure $ countDecisionPoints pt === countDecisionPoints pt' .&&. countDecisionPoints pt === 1
    describe "runAntiGen" $ do
      prop "zapping `antiGenPositive` once generates negative examples" $ do
        x <- runAntiGen 1 antiGenPositive
        pure $ x <= 0
      prop "zapping `antiGenPositive` zero times generates a positive example" $ do
        x <- runAntiGen 0 antiGenPositive
        pure $ x > 0
      prop "zapping `antiGenTuple` once results in a single non-positive Int" $ do
        (x, y) <- runAntiGen 1 antiGenTuple
        pure $
          label "x is non-positive" (x <= 0) .||. label "y is non-positive" (y <= 0)
      prop "zapping `antiGenTuple` twice results in two non-positive Ints" $ do
        (x, y) <- runAntiGen 2 antiGenTuple
        pure $
          counterexample ("x = " <> show x <> " is positive") (x <= 0)
            .&&. counterexample ("y = " <> show y <> " is positive") (y <= 0)
      prop
        "zapping the length of the string propagates to the string generator"
        . forAll (runAntiGen 1 antiGenLengthStringStatic)
        $ \(l, s) -> length s === l
      prop
        "zapping `antiGenLengthString` either generates invalid Int or a string of invalid length"
        . forAll (runAntiGen 1 antiGenLengthString)
        $ \(l, s) ->
          exactlyOne
            [ l > 5
            , length s /= l
            ]
      prop
        "zapping `antiGenEither` once gives a nice distribution"
        . forAll (runAntiGen 1 antiGenEither)
        $ \x ->
          exactlyOne
            [ case x of
                Right _ -> False
                Left v -> v <= 0
            , case x of
                Left _ -> False
                Right v -> length (filter not v) == 1
            , case x of
                Left _ -> False
                Right v -> length v > 5
            ]
