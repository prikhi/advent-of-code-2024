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

module Day10 where

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
main = getInputAndSolve (parseInputRaw parseIntGrid) trailScoreSum trailRatingSum


-- SOLVE

trailScoreSum :: Array (Int, Int) Int -> Int
trailScoreSum grd =
    let trailheads = map fst . filter ((== 0) . snd) $ A.assocs grd
        allPaths = foldr (findTrails grd) S.empty trailheads
        distinctStartStop = S.fromList . map (head &&& last) $ S.toList allPaths
     in S.size distinctStartStop


trailRatingSum :: Array (Int, Int) Int -> Int
trailRatingSum grd =
    let trailheads = map fst . filter ((== 0) . snd) $ A.assocs grd
     in S.size $ foldr (findTrails grd) S.empty trailheads


-- HELPERS

findTrails :: Array (Int, Int) Int -> (Int, Int) -> Set [(Int, Int)] -> Set [(Int, Int)]
findTrails grd startPos foundTrails =
    foldr S.insert foundTrails $ go startPos 0
  where
    go :: (Int, Int) -> Int -> [[(Int, Int)]]
    go pos curVal
        | curVal == 9 =
            [[pos]]
        | otherwise =
            let nextVal = succ curVal
                matchingNeighbors =
                    filter ((== nextVal) . snd)
                        . map (id &&& (grd A.!))
                        $ A.getGridNeighborsCardinal grd pos
             in map (pos :) $ concatMap (uncurry go) matchingNeighbors

-- PARSE
