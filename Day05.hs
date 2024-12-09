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

module Day05 where

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
main = getInputAndSolve (parseInputRaw parsePrintQueue) middleSum middleSumFixed


-- SOLVE

middleSum :: PrintQueue -> Int
middleSum PrintQueue {..} =
    sum $ map getMiddle $ filter (isValid orderingRules) updates


middleSumFixed :: PrintQueue -> Int
middleSumFixed PrintQueue {..} =
    sum $ map (getMiddle . fixUpdate) $ filter (not . isValid orderingRules) updates
  where
    fixUpdate :: [Int] -> [Int]
    fixUpdate originals =
        let withRules = map (\x -> (x, fromMaybe S.empty . M.lookup x $ fromRuleMap orderingRules)) originals
         in go [] withRules
    go :: [Int] -> [(Int, S.Set Int)] -> [Int]
    go placed = \case
        [] ->
            placed
        [(x, _)] ->
            placed <> [x]
        xr@(x, rules) : rest ->
            if correctPlace rules placed (map fst rest)
                then
                    go (placed <> [x]) rest
                else
                    go placed (rest <> [xr])


-- HELPERS

isValid :: RuleMap -> [Int] -> Bool
isValid (fromRuleMap -> ruleMap) = go []
  where
    go prevItems = \case
        [] ->
            True
        x : rest ->
            case M.lookup x ruleMap of
                Just followSet ->
                    correctPlace followSet prevItems rest
                        && go (prevItems <> [x]) rest
                Nothing ->
                    go (prevItems <> [x]) rest


correctPlace
    :: S.Set Int
    -- ^ Follow Set
    -> [Int]
    -- ^ Previous items
    -> [Int]
    -- ^ Following items
    -> Bool
correctPlace followSet prevItems nextItems =
    all (`S.member` followSet) nextItems
        && not (any (`S.member` followSet) prevItems)


getMiddle :: [Int] -> Int
getMiddle xs = xs !! floor @Rational (fromIntegral (length xs) / 2.0)


-- PARSE

data PrintQueue = PrintQueue
    { orderingRules :: !RuleMap
    , updates :: [[Int]]
    }
    deriving (Show)


parsePrintQueue :: ReadP PrintQueue
parsePrintQueue = do
    orderingRules <- fmap makeRuleMap . many1 $ parseRule <* newline
    void newline
    updates <- many1 $ parseIntArray Nothing Nothing <* newline
    eof
    return PrintQueue {..}
  where
    parseRule :: ReadP (Int, Int)
    parseRule =
        (,) <$> parseInt <* char '|' <*> parseInt


newtype RuleMap = RuleMap
    { fromRuleMap :: M.Map Int (S.Set Int)
    }
    deriving (Show)


makeRuleMap :: [(Int, Int)] -> RuleMap
makeRuleMap =
    RuleMap
        . foldr
            ( \(before, after) ->
                M.upsert
                    ( \case
                        Nothing -> S.fromList [after]
                        Just followSet -> S.insert after followSet
                    )
                    before
            )
            M.empty
