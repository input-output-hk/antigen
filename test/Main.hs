{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Test.AntiGen (countDecisionPoints, evalToPartial, sometimes)
import Test.Hspec (describe, hspec, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..), NonPositive (..), Positive (..), (.&&.), (===))

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
          g = (getPositive @Int <$> arbitrary) `sometimes` (getNonPositive <$> arbitrary)
          m = return =<< g
          m' = g
        pt <- evalToPartial m
        pt' <- evalToPartial m'
        pure $ countDecisionPoints pt === countDecisionPoints pt' .&&. countDecisionPoints pt === 1
