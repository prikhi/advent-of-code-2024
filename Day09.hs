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

module Day09 where

import Control.Arrow ((&&&))
import Control.Monad
import Control.Monad.ST (ST, runST)
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
import Data.Ord (Down (..))
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
main =
    getInputAndSolve
        (parseInputRaw $ many1 (read @Int . pure <$> satisfy isDigit) <* newline)
        compactChecksum
        defragChecksum


-- SOLVE

compactChecksum :: [Int] -> Int
compactChecksum diskMap =
    let initialDisk = A.fromList $ decompress diskMap
        compactedDisk = compact initialDisk
     in checksum compactedDisk


defragChecksum :: [Int] -> Int
defragChecksum diskMap =
    let (initialDisk, metadata) = first A.fromList $ decompressWithMetadata diskMap
     in checksum $ defragCorrect initialDisk metadata


-- HELPERS

decompress :: [Int] -> [Maybe Int]
decompress = go 0 True
  where
    go :: Int -> Bool -> [Int] -> [Maybe Int]
    go curFile isFile = \case
        [] ->
            []
        x : xs ->
            if isFile
                then
                    replicate x (Just curFile) <> go (succ curFile) (not isFile) xs
                else
                    replicate x Nothing <> go curFile (not isFile) xs


decompressWithMetadata :: [Int] -> ([Maybe Int], Map Int Metadata)
decompressWithMetadata = (\(_, _, _, xs, m) -> (xs, m)) . foldl' go (0, 0, True, [], M.empty)
  where
    go
        :: (Int, Int, Bool, [Maybe Int], Map Int Metadata)
        -> Int
        -> (Int, Int, Bool, [Maybe Int], Map Int Metadata)
    go (pos, curFileNum, isFile, processed, metadatas) x =
        if isFile
            then
                ( pos + x
                , succ curFileNum
                , not isFile
                , processed <> replicate x (Just curFileNum)
                , M.insert curFileNum Metadata {fileSize = x, startLoc = pos, endLoc = pos + x - 1} metadatas
                )
            else
                ( pos + x
                , curFileNum
                , not isFile
                , processed <> replicate x Nothing
                , metadatas
                )


data Metadata = Metadata
    { fileSize :: Int
    , startLoc :: Int
    , endLoc :: Int
    }
    deriving (Show)


compact :: Array Int (Maybe Int) -> Array Int (Maybe Int)
compact arr =
    let (writeHead, readHead) = A.bounds arr
     in runST $ A.thawSTArray arr >>= go writeHead readHead
  where
    go :: Int -> Int -> A.STArray s Int (Maybe Int) -> ST s (Array Int (Maybe Int))
    go writeHead readHead stArr = do
        writeVal <- A.readSTArray stArr writeHead
        readVal <- A.readSTArray stArr readHead
        if
            | readHead < writeHead ->
                A.freezeSTArray stArr
            | isJust writeVal ->
                go (succ writeHead) readHead stArr
            | isNothing readVal ->
                go writeHead (pred readHead) stArr
            | otherwise -> do
                A.writeSTArray stArr writeHead readVal
                A.writeSTArray stArr readHead Nothing
                go (succ writeHead) (pred readHead) stArr


defragCorrect :: Array Int (Maybe Int) -> Map Int Metadata -> Array Int (Maybe Int)
defragCorrect arr md =
    runST $ do
        stArr <- A.thawSTArray arr
        mapM_ (go stArr) . reverse $ M.toList md
        A.freezeSTArray stArr
  where
    go :: A.STArray s Int (Maybe Int) -> (Int, Metadata) -> ST s ()
    go stArr (fileNum, meta) = do
        mbSpanStart <- findEmptySpan stArr meta.fileSize 0 0 0
        forM_ mbSpanStart $ \spanStart -> when (spanStart < meta.startLoc) $ do
            forM_ (zip [0 ..] [meta.startLoc .. meta.endLoc]) $ \(offset, readLoc) -> do
                A.writeSTArray stArr (spanStart + offset) (Just fileNum)
                A.writeSTArray stArr readLoc Nothing

    findEmptySpan :: A.STArray s Int (Maybe Int) -> Int -> Int -> Int -> Int -> ST s (Maybe Int)
    findEmptySpan stArr targetSize currentSize start pos = do
        if not (A.inRange (A.bounds arr) pos)
            then
                return Nothing
            else do
                posVal <- A.readSTArray stArr pos
                if
                    | targetSize == currentSize ->
                        return $ Just start
                    | isJust posVal ->
                        findEmptySpan stArr targetSize 0 (succ pos) (succ pos)
                    | otherwise ->
                        findEmptySpan stArr targetSize (succ currentSize) start (succ pos)


-- | Oops, this does it the wrong way and produces results that are more
-- compact than expected. It finds spans and then puts the largest file ID
-- that'll fit in them, instead of going in desc order of file num & only
-- once.
defrag :: Array Int (Maybe Int) -> Map Int Metadata -> Array Int (Maybe Int)
defrag arr meta =
    let writeHead = fst $ A.bounds arr
     in runST $ A.thawSTArray arr >>= go writeHead S.empty
  where
    totalFileNums :: Int
    totalFileNums =
        M.size meta
    go :: Int -> Set Int -> A.STArray s Int (Maybe Int) -> ST s (Array Int (Maybe Int))
    go writeHead handledFileNums stArr = do
        writeVal <- A.readSTArray stArr writeHead
        if
            | S.size handledFileNums == totalFileNums ->
                A.freezeSTArray stArr
            | isJust writeVal ->
                go (succ writeHead) (S.insert (fromJust writeVal) handledFileNums) stArr
            | otherwise -> do
                emptySpanSize <- findEmptySpan stArr writeHead
                let filesThatFit =
                        L.sortOn (Down . fst)
                            . filter
                                ( \(k, md) ->
                                    not (S.member k handledFileNums)
                                        && md.fileSize <= emptySpanSize
                                )
                            $ M.toList meta
                case listToMaybe filesThatFit of
                    Nothing ->
                        go
                            (writeHead + emptySpanSize)
                            handledFileNums
                            stArr
                    Just (fileNum, md) -> do
                        forM_ (zip [0 ..] [md.startLoc .. md.endLoc]) $ \(offset, readLoc) -> do
                            A.writeSTArray stArr (writeHead + offset) (Just fileNum)
                            A.writeSTArray stArr readLoc Nothing
                        go
                            (writeHead + md.fileSize)
                            (S.insert fileNum handledFileNums)
                            stArr
    findEmptySpan :: A.STArray s Int (Maybe Int) -> Int -> ST s Int
    findEmptySpan stArr ix = do
        val <- A.readSTArray stArr ix
        if isJust val
            then
                return 0
            else
                succ <$> findEmptySpan stArr (succ ix)


checksum :: Array Int (Maybe Int) -> Int
checksum = sum . map (\(ix, f) -> maybe 0 (ix *) f) . A.assocs

-- PARSE
