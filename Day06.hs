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

module Day06 where

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
main = getInputAndSolve (parseInputRaw $ parseCharGrid (`elem` ".^#") <* newline) countDistinctPos countLoops


-- SOLVE
countDistinctPos :: A.Array (Int, Int) Char -> Int
countDistinctPos grd =
    let initialPos = fst . head . filter ((== '^') . snd) $ A.assocs grd
     in S.size $ tracePath grd (0, -1) (S.fromList [initialPos]) initialPos


countLoops :: A.Array (Int, Int) Char -> Int
countLoops grd =
    let initialPos = fst . head . filter ((== '^') . snd) $ A.assocs grd
        initialPath = S.toList $ tracePath grd (0, -1) (S.fromList [initialPos]) initialPos
     in length $
            filter
                ( \pos ->
                    pos /= initialPos
                        && hasLoop
                            (A.set [(pos, '#')] grd)
                            (0, -1)
                            (S.fromList [(initialPos, (0, -1))])
                            initialPos
                )
                initialPath


-- HELPERS

-- | Nothing if looped, Just if exits
tracePath
    :: A.Array (Int, Int) Char
    -- ^ Grid
    -> (Int, Int)
    -- ^ Direction
    -> S.Set (Int, Int)
    -- ^ Seen
    -> (Int, Int)
    -- ^ Pos
    -> S.Set (Int, Int)
tracePath grd dir@(dx, dy) visited pos@(x, y) =
    let forwardPos = (x + dx, y + dy)
        rotatedDir = rotate dir
     in if
            | not (A.inRange (A.bounds grd) forwardPos) ->
                visited
            | grd A.! forwardPos == '#' ->
                tracePath grd rotatedDir visited pos
            | otherwise ->
                tracePath grd dir (S.insert forwardPos visited) forwardPos


hasLoop
    :: A.Array (Int, Int) Char
    -- ^ Grid
    -> (Int, Int)
    -- ^ Direction
    -> S.Set ((Int, Int), (Int, Int))
    -- ^ Seen (pos, direction)
    -> (Int, Int)
    -- ^ Pos
    -> Bool
hasLoop grd dir@(dx, dy) visited pos@(x, y) =
    let forwardPos = (x + dx, y + dy)
        rotatedDir = rotate dir
     in if
            | not (A.inRange (A.bounds grd) forwardPos) ->
                False
            | grd A.! forwardPos == '#' ->
                hasLoop grd rotatedDir visited pos
            | otherwise ->
                S.member (forwardPos, dir) visited
                    || hasLoop grd dir (S.insert (forwardPos, dir) visited) forwardPos


rotate :: (Int, Int) -> (Int, Int)
rotate = \case
    (0, -1) ->
        (1, 0)
    (1, 0) ->
        (0, 1)
    (0, 1) ->
        (-1, 0)
    (-1, 0) ->
        (0, -1)
    other ->
        error $ "rotateDir: unsupported direction vector: " <> show other

-- PARSE
