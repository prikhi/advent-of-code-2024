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

module Day03 where

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
main = getInputAndSolve (parseInputRaw parseInstructions) runSimple runConditional


-- SOLVE

runSimple :: [Instruction] -> Int
runSimple =
    sum
        . map
            ( \case
                Multiply {..} -> xVal * yVal
                _ -> 0
            )


runConditional :: [Instruction] -> Int
runConditional =
    snd
        . foldl'
            ( \(isEnabled, acc) -> \case
                Do -> (True, acc)
                Dont -> (False, acc)
                Multiply {..} ->
                    if isEnabled
                        then
                            (isEnabled, xVal * yVal + acc)
                        else
                            (isEnabled, acc)
            )
            (True, 0)


-- HELPERS

-- PARSE

data Instruction
    = Multiply
        { xVal :: !Int
        , yVal :: !Int
        }
    | Dont
    | Do
    deriving (Show)


parseInstructions :: ReadP [Instruction]
parseInstructions = do
    catMaybes
        <$> manyTill
            ( (Just <$> parseMultiply)
                <++ (Just <$> parseDoDont)
                <++ ((satisfy (`notElem` "md") <++ get) $> Nothing)
            )
            eof
  where
    parseMultiply :: ReadP Instruction
    parseMultiply = do
        void $ string "mul("
        xVal <- parseInt
        void $ char ','
        yVal <- parseInt
        void $ char ')'
        return Multiply {..}
    parseDoDont :: ReadP Instruction
    parseDoDont =
        choice
            [ string "don't()" $> Dont
            , string "do()" $> Do
            ]
