{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.DeepSeq (deepseq)
import Criterion.Main (Benchmarkable, bench, bgroup, defaultMain, nfIO)
import qualified Data.Text as T
import Test.AntiGen.Internal (
  AntiGen,
  ZapResult (..),
  evalPartial,
  evalToPartial,
  withAnnotation,
  zapAt,
  (|!),
 )
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

-- Version with annotations on each decision point
bindListAnnotated :: Int -> AntiGen [Int]
bindListAnnotated 1 = (: []) <$> liftGen arbitrary
bindListAnnotated n
  | n <= 0 = pure []
  | otherwise = do
      rest <- bindListAnnotated (n - 1)
      case rest of
        x : xs -> do
          y <- withAnnotation (T.pack (show n)) $ pure (succ x) |! pure (pred x)
          pure $ y : x : xs
        [] -> error "Got empty list"

-- Only force the value
bindListZapValue :: Int -> Int -> Benchmarkable
bindListZapValue len i =
  nfIO . generate . variant (12345 :: Int) . fmap (evalPartial . zrValue) $
    zapAt (fromIntegral i) =<< evalToPartial (bindList len)

-- Force value, annotation, and zapped count
bindListZapAll :: Int -> Int -> Benchmarkable
bindListZapAll len i =
  nfIO . generate . variant (12345 :: Int) . fmap forceAll $
    zapAt (fromIntegral i) =<< evalToPartial (bindList len)
  where
    forceAll ZapResult {..} =
      zrAnnotation `deepseq` (evalPartial zrValue, zrZapped)

-- Annotated versions
annotatedZapValue :: Int -> Int -> Benchmarkable
annotatedZapValue len i =
  nfIO . generate . variant (12345 :: Int) . fmap (evalPartial . zrValue) $
    zapAt (fromIntegral i) =<< evalToPartial (bindListAnnotated len)

annotatedZapAll :: Int -> Int -> Benchmarkable
annotatedZapAll len i =
  nfIO . generate . variant (12345 :: Int) . fmap forceAll $
    zapAt (fromIntegral i) =<< evalToPartial (bindListAnnotated len)
  where
    forceAll ZapResult {..} =
      zrAnnotation `deepseq` (evalPartial zrValue, zrZapped)

main :: IO ()
main =
  defaultMain
    [ bgroup
        "value only"
        [ bench "10_000 zap at 0" $ bindListZapValue 10_000 0
        , bench "10_000 zap at 9_000" $ bindListZapValue 10_000 9_000
        , bench "1_000_000 zap at 0" $ bindListZapValue 1_000_000 0
        , bench "1_000_000 zap at 900_000" $ bindListZapValue 1_000_000 900_000
        ]
    , bgroup
        "force all"
        [ bench "10_000 zap at 0" $ bindListZapAll 10_000 0
        , bench "10_000 zap at 9_000" $ bindListZapAll 10_000 9_000
        , bench "1_000_000 zap at 0" $ bindListZapAll 1_000_000 0
        , bench "1_000_000 zap at 900_000" $ bindListZapAll 1_000_000 900_000
        ]
    , bgroup
        "annotated value only"
        [ bench "10_000 zap at 0" $ annotatedZapValue 10_000 0
        , bench "10_000 zap at 9_000" $ annotatedZapValue 10_000 9_000
        ]
    , bgroup
        "annotated force all"
        [ bench "10_000 zap at 0" $ annotatedZapAll 10_000 0
        , bench "10_000 zap at 9_000" $ annotatedZapAll 10_000 9_000
        ]
    ]
