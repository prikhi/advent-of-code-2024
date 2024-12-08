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

module Day01 where

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

import Data.Monoid (Sum (..))
import Debug.Trace


-- (parseInput lineParser) OR (parseInputRaw fullInputParser)
main :: IO ()
main = getInputAndSolve (parseInputRaw parseDigitList) sumDifferences similarityScore


-- SOLVE

sumDifferences :: Lists -> Int
sumDifferences Lists {left, right} = sum $ map abs $ zipWith (-) left right


similarityScore :: Lists -> Int
similarityScore Lists {left, right} =
    getSum $ foldMap (\x -> Sum $ (x *) $ fromMaybe 0 $ M.lookup x countMap) left
  where
    countMap :: M.Map Int Int
    countMap =
        foldr (M.upsert (maybe 1 succ)) M.empty right


-- HELPERS

-- PARSE
data Lists = Lists
    { left :: [Int]
    , right :: [Int]
    }
    deriving (Show)


parseDigitList :: ReadP Lists
parseDigitList = do
    pairs <- many1 $ do
        x <- parseInt
        skipSpaces
        y <- parseInt
        newline
        pure [x, y]
    eof
    let [L.sort -> left, L.sort -> right] = L.transpose pairs
    return Lists {..}
