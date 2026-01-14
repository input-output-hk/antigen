{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad (replicateM)
import Test.AntiGen (AntiGen, runAntiGen, zapAntiGen, (|!))
import Test.AntiGen.Internal (countDecisionPoints, evalToPartial)
import Test.Hspec (describe, hspec, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (
  Arbitrary (..),
  Gen,
  NonNegative (..),
  NonPositive (..),
  Positive (..),
  Property,
  Testable (..),
  counterexample,
  forAll,
  forAllBlind,
  label,
  oneof,
  scale,
  suchThat,
  (.&&.),
  (.||.),
  (===),
 )
import Test.QuickCheck.GenT (MonadGen (..))

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
  genLeft <- liftGen arbitrary
  if genLeft
    then Left <$> antiGenPositive
    else
      Right <$> do
        l <- antiGenSmall
        replicateM l $ pure True |! pure False

noneOf :: [Bool] -> Property
noneOf [] = property True
noneOf (x : xs) = not x .&&. noneOf xs

exactlyOne :: [(String, Bool)] -> Property
exactlyOne [] = counterexample "None of the conditions hold" $ property False
exactlyOne ((lbl, p) : ps) = label lbl (p .&&. noneOf (snd <$> ps)) .||. (not p .&&. exactlyOne ps)

someGen :: Gen (Gen Int)
someGen =
  oneof
    [ pure <$> arbitrary
    , do
        x <- scale (`div` 2) someGen
        f <- arbitrary
        pure $ f <$> x
    , do
        x <- scale (`div` 4) someGen
        y <- scale (`div` 4) someGen
        f <- arbitrary
        pure $ f <$> x <*> y
    ]

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
      prop "runAntiGen . liftGen == id" $
        \seed -> forAllBlind someGen $ \g -> do
          let g' = variant (seed :: Int) $ runAntiGen (liftGen g)
          res <- variant seed g
          res' <- variant seed g'
          pure $ res === res'
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
