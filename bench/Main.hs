{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Criterion.Main (Benchmarkable, bench, defaultMain, nfIO)
import Test.AntiGen.Internal (AntiGen, evalPartial, evalToPartial, zapAt, (|!))
import Test.QuickCheck (Arbitrary (..), generate)
import Test.QuickCheck.GenT (MonadGen (..))

bindList :: Int -> AntiGen [Int]
bindList 1 = (: []) <$> liftGen arbitrary
bindList n
  | n <= 0 = pure []
  | otherwise = do
      rest <- bindList (n - 1)
      case rest of
        x : xs -> do
          y <- pure (succ x) |! pure (pred x)
          pure $ y : x : xs
        [] -> error "Got empty list"

bindListZap :: Int -> Int -> Benchmarkable
bindListZap len i =
  nfIO . generate . variant (12345 :: Int) . fmap evalPartial $
    zapAt i =<< evalToPartial (bindList len)

main :: IO ()
main =
  defaultMain
    [ bench "bindList 10_000 zap at 0" $ bindListZap 10_000 0
    , bench "bindList 10_000 zap at 9_000" $ bindListZap 10_000 9_000
    , bench "bindList 1_000_000 zap at 0" $ bindListZap 1_000_000 0
    , bench "bindList 1_000_000 zap at 900_000" $ bindListZap 1_000_000 900_000
    ]
