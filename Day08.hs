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

module Day08 where

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
main =
    getInputAndSolve
        (parseInputRaw $ parseCharGrid (\c -> isAlphaNum c || c == '.') <* newline)
        uniqueAntinodes
        uniqueInlineAntinodes


-- SOLVE

uniqueAntinodes :: Array (Int, Int) Char -> Int
uniqueAntinodes =
    countUniqueAntinodes $
        \thisNode otherNode ->
            [ potentialAntinodeLocation thisNode otherNode
            , potentialAntinodeLocation otherNode thisNode
            ]
  where
    potentialAntinodeLocation :: (Int, Int) -> (Int, Int) -> (Int, Int)
    potentialAntinodeLocation (x1, y1) (x2, y2) =
        let dx = x2 - x1
            dy = y2 - y1
         in ( x2 + dx
            , y2 + dy
            )


uniqueInlineAntinodes :: Array (Int, Int) Char -> Int
uniqueInlineAntinodes grd =
    countUniqueAntinodes
        ( \thisNode otherNode ->
            lineToBounds thisNode otherNode
                <> lineToBounds otherNode thisNode
                <> [thisNode, otherNode]
        )
        grd
  where
    bounds = A.bounds grd
    lineToBounds :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
    lineToBounds (x1, y1) n@(x2, y2) =
        let dx = x2 - x1
            dy = y2 - y1
         in extrapolate (dx, dy) n
    extrapolate :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
    extrapolate dxdy@(dx, dy) (x, y) =
        let newNode = (x + dx, y + dy)
         in if A.inRange bounds newNode
                then
                    newNode : extrapolate dxdy newNode
                else []


-- HELPERS

countUniqueAntinodes :: ((Int, Int) -> (Int, Int) -> [(Int, Int)]) -> Array (Int, Int) Char -> Int
countUniqueAntinodes antinodeGen grd =
    S.size $ foldl' (findAntinodes grd antinodeGen) S.empty $ map snd $ nodeLocations grd


nodeLocations :: Array (Int, Int) Char -> [(Char, [(Int, Int)])]
nodeLocations grd =
    filter ((/= '.') . fst)
        $ M.toList
        $ foldr
            ( \(ix, el) ->
                M.upsert
                    ( \case
                        Nothing -> [ix]
                        Just ixs -> ix : ixs
                    )
                    el
            )
            M.empty
        $ A.assocs grd


findAntinodes
    :: Array (Int, Int) Char
    -> ((Int, Int) -> (Int, Int) -> [(Int, Int)])
    -> Set (Int, Int)
    -> [(Int, Int)]
    -> Set (Int, Int)
findAntinodes grd@(A.bounds -> gridRange) antinodeGen knownNodes = \case
    [] ->
        knownNodes
    thisNode : rest ->
        let foundAntinodes = do
                otherNode <- rest
                potentialAntinode <- antinodeGen thisNode otherNode
                guard $ A.inRange gridRange potentialAntinode
                pure potentialAntinode
            newKnown = foldr S.insert knownNodes foundAntinodes
         in findAntinodes grd antinodeGen newKnown rest

-- PARSE
