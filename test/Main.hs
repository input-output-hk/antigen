{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Test.AntiGen (AntiGen, countDecisionPoints, evalToPartial, runAntiGen, sometimes)
import Test.Hspec (describe, hspec, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (
  Arbitrary (..),
  NonPositive (..),
  Positive (..),
  counterexample,
  (.&&.),
  (===), label, (.||.),
 )

antiGenPositive :: AntiGen Int
antiGenPositive = (getPositive @Int <$> arbitrary) `sometimes` (getNonPositive <$> arbitrary)

antiGenTuple :: AntiGen (Int, Int)
antiGenTuple = do
  x <- antiGenPositive
  y <- antiGenPositive
  pure (x, y)

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
