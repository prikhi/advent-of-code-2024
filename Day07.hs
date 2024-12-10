{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Day07 where

import Control.Arrow ((&&&))
import Control.Monad
import Data.Array (Array)
import Data.Bifunctor
import Data.Char
import Data.Either
import Data.Foldable
import Data.Function (on)
import Data.Functor
import Data.Map (Map)
import Data.Maybe
import Data.MultiSet (MultiSet)
import Data.Set (Set)
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import Data.Array qualified as A
import Data.List qualified as L
import Data.Map qualified as M
import Data.MultiSet qualified as MS
import Data.Set qualified as S

import Debug.Trace


-- (parseInput lineParser) OR (parseInputRaw fullInputParser)
main :: IO ()
main = getInputAndSolve (parseInput parseCalibration) (totalCalibrationResult validOperators) (totalCalibrationResult validWithConcat)


-- SOLVE
totalCalibrationResult :: (Calibration -> Bool) -> [Calibration] -> Int
totalCalibrationResult validator = sum . map target . filter validator


-- HELPERS

validOperators :: Calibration -> Bool
validOperators Calibration {target, numbers} =
    elem target $ uncurry iterateOperations $ pure . head &&& tail $ numbers
  where
    iterateOperations :: [Int] -> [Int] -> [Int]
    iterateOperations accs = \case
        [] -> accs
        x : xs -> do
            acc <- accs
            res <- [acc * x, acc + x]
            iterateOperations [res] xs


validWithConcat :: Calibration -> Bool
validWithConcat Calibration {target, numbers} =
    elem target $ uncurry iterateOperations $ pure . head &&& tail $ numbers
  where
    iterateOperations :: [Int] -> [Int] -> [Int]
    iterateOperations accs = \case
        [] -> accs
        x : xs -> do
            acc <- accs
            res <- [acc * x, acc + x, read $ show acc <> show x]
            iterateOperations [res] xs


-- PARSE

data Calibration = Calibration
    { target :: Int
    , numbers :: [Int]
    }
    deriving (Show)


parseCalibration :: ReadP Calibration
parseCalibration = do
    target <- parseInt
    void $ string ": "
    numbers <- sepBy1 parseInt (char ' ')
    return Calibration {..}
