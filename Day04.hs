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

module Day04 where

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
main = getInputAndSolve (parseInputRaw $ parseCharGrid isAlpha <* newline) findWords findCrosses


-- SOLVE

findWords :: A.Array (Int, Int) Char -> Int
findWords grd =
    sum $ map wordSearch xLocations
  where
    xLocations :: [(Int, Int)]
    xLocations = map fst . filter ((== 'X') . snd) $ A.assocs grd
    wordSearch :: (Int, Int) -> Int
    wordSearch = go Nothing
    go :: Maybe ((Int, Int), Char) -> (Int, Int) -> Int
    go mbDirectionTarget coords@(xCoord, yCoord) = case mbDirectionTarget of
        Nothing ->
            let neighbors = A.getGridNeighborsDiagonal grd coords
                targetNeighbors = map fst . filter ((== 'M') . snd) $ map (id &&& (grd A.!)) neighbors
             in sum $
                    map
                        ( \nextCoord@(nextX, nextY) ->
                            let direction = (nextX - xCoord, nextY - yCoord)
                             in go (Just (direction, 'A')) nextCoord
                        )
                        targetNeighbors
        Just (dir@(xDelta, yDelta), target) ->
            let nextCoord = (xCoord + xDelta, yCoord + yDelta)
                mbNextVal = if nextCoord `elem` A.getGridNeighborsDiagonal grd coords then Just (grd A.! nextCoord) else Nothing
             in case mbNextVal of
                    Nothing ->
                        0
                    Just nextVal
                        | nextVal == target && target == 'S' ->
                            1
                        | nextVal == target ->
                            go (Just (dir, 'S')) nextCoord
                        | otherwise ->
                            0


findCrosses :: A.Array (Int, Int) Char -> Int
findCrosses grd =
    sum $ map crossSearch aLocations
  where
    aLocations :: [(Int, Int)]
    aLocations =
        filter (\(x, y) -> x `notElem` [0, width] && y `notElem` [0, height]) $
            map fst . filter ((== 'A') . snd) $
                A.assocs grd
    (_, (width, height)) = A.bounds grd
    crossSearch :: (Int, Int) -> Int
    crossSearch (xCoord, yCoord) =
        let lookDiff (dx, dy) = grd A.! (xCoord + dx, yCoord + dy)
            upperLeft = lookDiff (-1, -1)
            upperRight = lookDiff (1, -1)
            lowerLeft = lookDiff (-1, 1)
            lowerRight = lookDiff (1, 1)
         in if all (`elem` [upperLeft, lowerRight]) ['M', 'S']
                && all (`elem` [lowerLeft, upperRight]) ['M', 'S']
                then 1
                else 0

-- HELPERS

-- PARSE
