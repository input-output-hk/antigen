{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad (replicateM)
import Data.Data (Proxy (..))
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as T
import Data.Word (Word32, Word64, Word8)
import Paths_antigen (getDataDir)
import System.FilePath ((</>))
import Test.AntiGen (
  AntiGen,
  antiChoose,
  antiChooseBounded,
  antiNegative,
  antiNonNegative,
  antiNonPositive,
  antiPositive,
  faultyBool,
  faultyNum,
  faultyTry,
  replicateMNorm,
  runAntiGen,
  zapAntiGen,
  (|!),
  (||!),
 )
import Test.AntiGen.Internal (
  ZapResult (..),
  countDecisionPoints,
  evalToPartial,
  prettyZapResult,
  reweigh,
  withAnnotation,
  zapAntiGenResult,
 )
import Test.Hspec (Spec, describe, hspec, it, shouldBe, shouldSatisfy)
import Test.Hspec.Golden (Golden (..))
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
import Type.Reflection (Typeable, typeRep)

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

annotatedPositive :: AntiGen Int
annotatedPositive =
  withAnnotation "must be positive" $
    (getPositive @Int <$> arbitrary) |! (getNonPositive <$> arbitrary)

annotatedTuple :: AntiGen (Int, Int)
annotatedTuple = do
  x <-
    withAnnotation "first positive" $ (getPositive @Int <$> arbitrary) |! (getNonPositive <$> arbitrary)
  y <-
    withAnnotation "second positive" $
      (getPositive @Int <$> arbitrary) |! (getNonPositive <$> arbitrary)
  pure (x, y)

complexAnnotations :: AntiGen (Int, Int)
complexAnnotations =
  withAnnotation "root" $ do
    a <- withAnnotation "a" antiPositive
    b <- withAnnotation "b" antiPositive
    pure (a, b)

-- | Annotated sum type - each branch has its own annotation
annotatedEither :: AntiGen (Either Int Int)
annotatedEither =
  withAnnotation "either" $
    oneof
      [ withAnnotation "left" $ Left <$> antiPositive
      , withAnnotation "right" $ Right <$> antiNegative
      ]

-- | Three levels of nesting
deeplyNested :: AntiGen Int
deeplyNested =
  withAnnotation "level1" $
    withAnnotation "level2" $
      withAnnotation "level3" $
        antiPositive

-- | Mixed annotated and unannotated in sequence
mixedAnnotations :: AntiGen (Int, Int, Int)
mixedAnnotations = do
  a <- withAnnotation "first" antiPositive
  b <- antiPositive -- unannotated
  c <- withAnnotation "third" antiPositive
  pure (a, b, c)

-- | Sibling scopes that don't inherit from each other
siblingScopes :: AntiGen (Int, Int)
siblingScopes = do
  a <- withAnnotation "scopeA" antiPositive
  b <- withAnnotation "scopeB" antiPositive
  pure (a, b)

-- | Annotation wrapping a pure value (no decision points inside)
annotatedPure :: AntiGen Int
annotatedPure = withAnnotation "pure" $ pure 42

-- | Annotation with decision point after the annotated section
annotationThenDecision :: AntiGen (Int, Int)
annotationThenDecision = do
  a <- withAnnotation "annotated" antiPositive
  b <- antiPositive -- should have empty annotation
  pure (a, b)

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

chooseBoundedIntegralTest :: forall a. Typeable a => Spec
chooseBoundedIntegralTest =
  prop (show (typeRep @a) <> " (0, n)") $ \(Positive (n :: Word64)) -> do
    res <- zapAntiGen 1 (antiChooseBounded (0, n))
    pure $ res `shouldSatisfy` (> n)

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
      prop "positive" $ forAll (runAntiGen $ antiPositive @Int) (> 0)
      prop "negative" $ forAll (zapAntiGen 1 $ antiPositive @Int) (<= 0)
    describe "antiNegative" $ do
      prop "positive" $ forAll (runAntiGen $ antiNegative @Int) (< 0)
      prop "negative" $ forAll (zapAntiGen 1 $ antiNegative @Int) (>= 0)
    describe "antiNonPositive" $ do
      prop "positive" $ forAll (runAntiGen $ antiNonPositive @Int) (<= 0)
      prop "negative" $ forAll (zapAntiGen 1 $ antiNonPositive @Int) (> 0)
    describe "antiNonNegative" $ do
      prop "positive" $ forAll (runAntiGen $ antiNonNegative @Int) (>= 0)
      prop "negative" $ forAll (zapAntiGen 1 $ antiNonNegative @Int) (< 0)
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
    describe "antiChoose" $ do
      prop "positive and negative" $ do
        vals <- sort <$> replicateM 4 (arbitrary @Word8)
        case vals of
          [loB, lo, hi, hiB] -> do
            let g = antiChoose @Word8 (lo, hi) (loB, hiB)
            pos <- runAntiGen g
            neg <- zapAntiGen 1 g
            let
              failMsg =
                unlines
                  [ "loB = " <> show loB
                  , "hiB = " <> show hiB
                  , "lo = " <> show lo
                  , "hi = " <> show hi
                  , "pos = " <> show pos
                  , "neg = " <> show neg
                  ]
              posGood = pos >= lo && pos <= hi
              negGood = neg < lo || (neg >= hi && neg <= hiB)
              trivial = lo == loB && hi == hiB
            pure . counterexample failMsg $
              trivial .||. counterexample "pos failure" posGood .&&. counterexample "neg failure" negGood
          _ -> error "Impossible happened"
    describe "chooseBoundedIntegral" $ do
      chooseBoundedIntegralTest @Word64
      chooseBoundedIntegralTest @Word32
      chooseBoundedIntegralTest @Int
    describe "replicateMNorm" $ do
      prop "behaves like replicateM when not zapped" . forAll (choose (0, 1000)) $ \n -> do
        let gen = antiPositive @Int
        fromNorm <- runAntiGen $ replicateMNorm n gen
        fromReplicateM <- runAntiGen $ replicateM n gen
        pure $
          counterexample ("replicateMNorm: " <> show fromNorm) $
            counterexample ("replicateM: " <> show fromReplicateM) $
              length fromNorm === length fromReplicateM
      prop "produces correct length" . forAll (choose (0, 1000)) $ \n -> do
        result <- runAntiGen $ replicateMNorm n (antiPositive @Int)
        pure $ length result === n
      prop "all elements satisfy the generator property when not zapped" $ do
        n <- choose (0, 1000)
        result <- runAntiGen $ replicateMNorm n (antiPositive @Int)
        pure $ all (> 0) result
    describe "reweigh" $ do
      prop "zero weight generator is never zapped" $ do
        let gen = do
              x <- reweigh 0 $ pure "A" |! pure "a"
              y <- pure "B" |! pure "b"
              pure (x, y)
        (x, y) <- zapAntiGen 1 gen
        pure $
          counterexample ("x = " <> x <> ", y = " <> y) $
            x === "A" .&&. y === "b"

withAnnotationSpec :: Spec
withAnnotationSpec =
  describe "withAnnotation" $ do
    prop "behaves like (|!) for runAntiGen (active generator)" $ do
      x <- runAntiGen annotatedPositive
      pure $ x > 0
    prop "behaves like (|!) for zapAntiGen (alternative generator)" $ do
      x <- zapAntiGen 1 annotatedPositive
      pure $ x <= 0
    prop "annotation is captured in ZapResult when zapped" $ do
      result <- zapAntiGenResult 1 annotatedPositive
      pure $
        counterexample ("annotations: " <> show (zrAnnotation result)) $
          zrAnnotation result === ["must be positive" :| []]
    prop "no annotation in ZapResult when not zapped (n=0)" $ do
      result <- zapAntiGenResult 0 annotatedPositive
      pure $ zrAnnotation result === []
    prop "multiple annotations collected when zapping multiple points" $ do
      result <- zapAntiGenResult 2 annotatedTuple
      pure $
        counterexample ("annotations: " <> show (zrAnnotation result)) $
          length (zrAnnotation result) === 2
            .&&. ("first positive" :| []) `elem` zrAnnotation result
            .&&. ("second positive" :| []) `elem` zrAnnotation result
    prop "single zap of composed annotated generator gets one annotation" $ do
      result <- zapAntiGenResult 1 annotatedTuple
      pure $
        counterexample ("annotations: " <> show (zrAnnotation result)) $
          length (zrAnnotation result) === 1
    prop "non-annotated (|!) produces empty annotation list" $ do
      result <- zapAntiGenResult 1 antiGenPositive
      pure $ zrAnnotation result === []
    prop "nested withAnnotation produces hierarchical path" $ do
      let nested =
            withAnnotation "foo" $
              withAnnotation "bar" $
                (getPositive @Int <$> arbitrary) |! (getNonPositive <$> arbitrary)
      result <- zapAntiGenResult 1 nested
      pure $
        counterexample ("annotations: " <> show (zrAnnotation result)) $
          zrAnnotation result === ["foo" :| ["bar"]]
    prop "withAnnotation on unannotated produces single-element path" $ do
      let annotated = withAnnotation "outer" $ (getPositive @Int <$> arbitrary) |! (getNonPositive <$> arbitrary)
      result <- zapAntiGenResult 1 annotated
      pure $
        counterexample ("annotations: " <> show (zrAnnotation result)) $
          zrAnnotation result === ["outer" :| []]
    prop "complexAnnotations: zapping both points shows hierarchical paths" $ do
      result <- zapAntiGenResult 2 complexAnnotations
      pure $
        counterexample ("annotations: " <> show (zrAnnotation result)) $
          length (zrAnnotation result) === 2
            .&&. ("root" :| ["a"]) `elem` zrAnnotation result
            .&&. ("root" :| ["b"]) `elem` zrAnnotation result
    prop "annotatedEither: sum type branches have correct paths" $ do
      result <- zapAntiGenResult 1 annotatedEither
      pure $
        counterexample ("annotations: " <> show (zrAnnotation result)) $
          (zrAnnotation result === ["either" :| ["left"]])
            .||. (zrAnnotation result === ["either" :| ["right"]])
    prop "deeplyNested: three-level path captured" $ do
      result <- zapAntiGenResult 1 deeplyNested
      pure $
        counterexample ("annotations: " <> show (zrAnnotation result)) $
          zrAnnotation result === ["level1" :| ["level2", "level3"]]
    prop "mixedAnnotations: only annotated points produce annotations" $ do
      result <- zapAntiGenResult 3 mixedAnnotations
      pure $
        counterexample ("annotations: " <> show (zrAnnotation result)) $
          -- 3 decision points zapped, but only 2 have annotations
          zrZapped result === 3
            .&&. length (zrAnnotation result) === 2
            .&&. ("first" :| []) `elem` zrAnnotation result
            .&&. ("third" :| []) `elem` zrAnnotation result
    prop "siblingScopes: siblings don't inherit from each other" $ do
      result <- zapAntiGenResult 2 siblingScopes
      pure $
        counterexample ("annotations: " <> show (zrAnnotation result)) $
          length (zrAnnotation result) === 2
            .&&. ("scopeA" :| []) `elem` zrAnnotation result
            .&&. ("scopeB" :| []) `elem` zrAnnotation result
    prop "annotatedPure: no decision points means no annotations" $ do
      result <- zapAntiGenResult 1 annotatedPure
      pure $
        counterexample ("annotations: " <> show (zrAnnotation result)) $
          zrAnnotation result === []
            .&&. zrZapped result === 0
    prop "annotationThenDecision: scope ends after annotated section" $ do
      result <- zapAntiGenResult 2 annotationThenDecision
      pure $
        counterexample ("annotations: " <> show (zrAnnotation result)) $
          -- 2 decision points zapped, but only 1 has an annotation
          zrZapped result === 2
            .&&. length (zrAnnotation result) === 1
            .&&. ("annotated" :| []) `elem` zrAnnotation result

-- | Golden test that doesn't create actual files
golden :: String -> String -> IO (Golden String)
golden name actual = do
  dataDir <- getDataDir
  pure $
    Golden
      { output = actual
      , encodePretty = id
      , writeToFile = writeFile
      , readFromFile = readFile
      , goldenFile = dataDir </> ".golden" </> name ++ ".golden"
      , actualFile = Nothing
      , failFirstTime = False
      }

prettyZapResultSpec :: Spec
prettyZapResultSpec =
  describe "prettyZapResult" $ do
    it "no zaps" $
      golden "no_zaps" $
        T.unpack $
          prettyZapResult $
            ZapResult () [] 0
    it "single zap without annotation" $
      golden "single_zap_no_annotation" $
        T.unpack $
          prettyZapResult $
            ZapResult () [] 1
    it "single zap with simple annotation" $
      golden "single_zap_simple" $
        T.unpack $
          prettyZapResult $
            ZapResult () ["positive" :| []] 1
    it "single zap with nested annotation" $
      golden "single_zap_nested" $
        T.unpack $
          prettyZapResult $
            ZapResult () ["root" :| ["child", "leaf"]] 1
    it "multiple zaps with annotations" $
      golden "multiple_zaps" $
        T.unpack $
          prettyZapResult $
            ZapResult
              ()
              [ "user" :| ["name"]
              , "user" :| ["email"]
              , "address" :| ["street"]
              ]
              3
    it "zaps with mixed annotated and unannotated" $
      golden "mixed_annotations" $
        T.unpack $
          prettyZapResult $
            ZapResult () ["annotated" :| []] 3

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
    withAnnotationSpec
    prettyZapResultSpec
