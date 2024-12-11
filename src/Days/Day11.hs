{-# LANGUAGE BangPatterns #-}
module Days.Day11 (main) where

import qualified Data.Map.Strict as M
import Control.Monad (forM_)
import Data.List (foldl')

type Stone = Integer
type Depth = Int
type Cache = M.Map (Stone, Depth) Integer

transformSingleStone :: Stone -> [Stone]
transformSingleStone 0 = [1]
transformSingleStone stone = 
    if even (length digits)
    then [left, right]
    else [stone * 2024]
  where
    digits = show stone
    halfLen = length digits `div` 2
    (leftStr, rightStr) = splitAt halfLen digits
    left = read leftStr
    right = read rightStr

countStones :: Cache -> Stone -> Depth -> (Cache, Integer)
countStones cache stone depth
    | depth == 0 = (cache, 1)
    | Just result <- M.lookup (stone, depth) cache = (cache, result)
    | otherwise = 
        let newStones = transformSingleStone stone
            (finalCache, total) = foldl' go (cache, 0) newStones
            !finalCacheWithResult = M.insert (stone, depth) total finalCache
        in (finalCacheWithResult, total)
  where
    go (!currentCache, !acc) s = 
        let (newCache, count) = countStones currentCache s (depth - 1)
        in (newCache, acc + count)

processStones :: [Stone] -> Depth -> Integer
processStones initialStones depth = 
    snd $ foldl' countStone (M.empty, 0) initialStones
  where
    countStone (!cache, !total) stone = 
        let (newCache, count) = countStones cache stone depth
        in (newCache, total + count)

stones :: [Stone]
stones = [17639, 47, 3858, 0, 470624, 9467423, 5, 188]

main :: IO ()
main = do
    forM_ [1..75] $ \i -> do
        let result = processStones stones i
        putStrLn $ "After blink " ++ show i ++ ": " ++ show result