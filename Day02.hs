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

module Day02 where

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


main :: IO ()
main = getInputAndSolve (parseInputRaw parseReports) (countSafe pure) (countSafe droppedPermutations)


-- SOLVE

countSafe :: (Reports -> [Reports]) -> [Reports] -> Int
countSafe generate = length . filter anyIsSafe
  where
    anyIsSafe :: Reports -> Bool
    anyIsSafe reports =
        let permutations = generate reports
         in any (isRight . foldM isSafe (Nothing, Nothing) . fromReports) permutations
    isSafe :: (Maybe Bool, Maybe Int) -> Int -> Either () (Maybe Bool, Maybe Int)
    isSafe (mbIncreasing, mbLastVal) val = case (mbLastVal, mbIncreasing) of
        (Nothing, _) ->
            pure (mbIncreasing, Just val)
        (Just lastVal, Nothing) -> do
            let diff = val - lastVal
            when (diff < -3 || diff > 3 || diff == 0) $
                Left ()
            let increasing = diff > 0
            pure (Just increasing, Just val)
        (Just lastVal, Just isIncreasing) -> do
            let diff = val - lastVal
            when (diff > 0 && not isIncreasing) $
                Left ()
            when (diff < 0 && isIncreasing) $
                Left ()
            when (diff == 0) $
                Left ()
            when (abs diff > 3) $
                Left ()
            pure (mbIncreasing, Just val)


droppedPermutations :: Reports -> [Reports]
droppedPermutations Reports {..} =
    Reports <$> makePerms [] fromReports
  where
    makePerms :: [Int] -> [Int] -> [[Int]]
    makePerms prev = \case
        [] ->
            []
        l@(x : xs) ->
            [prev <> xs, prev <> l] <> makePerms (prev <> [x]) xs


-- HELPERS

-- PARSE
newtype Reports = Reports
    { fromReports :: [Int]
    }
    deriving (Show)


parseReports :: ReadP [Reports]
parseReports = manyTill (Reports <$> sepBy parseInt (char ' ') <* newline) eof
