module Days.Day19 (main) where

import Util (readInputFile)
import Data.List (sortBy)
import Data.Text (Text, pack, strip, splitOn)
import qualified Data.Text as T
import Data.Ord (comparing, Down(Down))
import qualified Data.Map.Strict as Map

-- Cache type for memoization
type Cache = Map.Map Text Integer

-- Main solver function with memoization
solveMemo :: Text -> [Text] -> Cache -> (Integer, Cache)
solveMemo str patterns cache
    | T.null str = (1, cache)  -- Base case: empty string means we found a solution
    | T.length str > 0 && null patterns = (0, cache)  -- No patterns left but string remains
    | otherwise = case Map.lookup str cache of
        Just result -> (result, cache)  -- Return cached result if available
        Nothing -> 
            let (result, newCache) = foldl processPattern (0, cache) patterns
                finalCache = Map.insert str result newCache
            in (result, finalCache)
  where
    processPattern (acc, c) pat
        | pat `T.isPrefixOf` str =
            let (subResult, newCache) = solveMemo (T.drop (T.length pat) str) patterns c
            in (acc + subResult, newCache)
        | otherwise = (acc, c)

-- Function to check if a solution exists (optimized)
solvable :: Text -> [Text] -> Bool
solvable str patterns
    | T.null str = True
    | T.length str > 0 && null patterns = False
    | otherwise = any matchAndContinue patterns
  where
    matchAndContinue pat =
        (pat `T.isPrefixOf` str) && solvable (T.drop (T.length pat) str) patterns

-- Count total solutions using memoization
countSolutions :: Text -> [Text] -> Integer
countSolutions str patterns = fst $ solveMemo str patterns Map.empty

-- Preprocess patterns for better performance
preprocessPatterns :: [Text] -> [Text]
preprocessPatterns = sortBy (comparing (Down . T.length))

main :: IO ()
main = do
    inputs <- readInputFile "day19.txt"
    towelsInput <- readInputFile "day19_patterns.txt"
    
    let towels = map strip $ splitOn (pack ",") (pack towelsInput)
        strs = map (T.filter (/= '\r') . pack) (lines inputs)
        sortedPatterns = preprocessPatterns towels
        
        -- Part 1: Check solvability
        part1Results = map (`solvable` sortedPatterns) strs
        part1Total = length $ filter id part1Results
        
        -- Part 2: Count solutions
        part2Results = map (`countSolutions` sortedPatterns) strs
        part2Total = sum part2Results
    
    putStrLn "Part 1:"
    print part1Total
    putStrLn "Part 2:"
    print part2Total