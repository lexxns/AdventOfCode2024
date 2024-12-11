module Days.Day09 (main) where

import Data.List (foldl')
import Data.Either (lefts)
import Util (readInputFile)
import Data.Char (digitToInt)
import Data.List (groupBy)

data Gap = Gap 
    { gapIdx :: Int
    , gapLen :: Int
    }

getData :: String -> [String]
getData input = concat $ zipWith processChar [0..] input
  where
    processChar i c
      | even i    = replicate (digitToInt c) (show (i `div` 2))
      | otherwise = replicate (digitToInt c) "."

findGaps :: [String] -> [Gap]
findGaps xs = 
    let indexed = zip [0..] xs
        isDot (_, x) = x == "."
        toGap xs = Gap (fst $ head xs) (length xs)
    in map toGap $ filter (isDot . head) $ groupBy (\a b -> isDot a && isDot b) indexed

findGroups :: [String] -> [(Int, Int, String)]
findGroups xs = 
    let indexed = zip [0..] xs
        notDot (_, x) = x /= "."
        toGroup g = (fst $ head g, length g, snd $ head g)
    in reverse $ map toGroup $ filter (notDot . head) $ 
       groupBy (\a b -> snd a == snd b && notDot a) indexed

tryMoveGroup :: [String] -> (Int, Int, String) -> [Gap] -> Maybe [String]
tryMoveGroup xs (pos, groupLen, val) gaps =
    case filter (\g -> gapIdx g < pos && gapLen g >= groupLen) gaps of
        [] -> Nothing
        (g:_) -> Just $ moveGroup xs pos groupLen val (gapIdx g)

moveGroup :: [String] -> Int -> Int -> String -> Int -> [String]
moveGroup xs start len val targetIdx =
    let before = take targetIdx xs
        middle = replicate len val
        gapFill = replicate len "."
        afterGap = drop (targetIdx + len) $ take start xs
        after = drop (start + len) xs
    in before ++ middle ++ afterGap ++ gapFill ++ after

checksum :: [String] -> Int
checksum xs = sum $ zipWith (*) [0..] (map getValue xs)
  where
    getValue "." = 0
    getValue x = read x

part2 :: String -> Int
part2 input = checksum $ processGroups (getData input)
  where
    processGroups xs = processGroupsOnce xs (findGroups xs)
    
    processGroupsOnce :: [String] -> [(Int, Int, String)] -> [String]
    processGroupsOnce current [] = current
    processGroupsOnce current (group:remainingGroups) =
        case tryMoveGroup current group (findGaps current) of
            Nothing -> processGroupsOnce current remainingGroups
            Just newState -> processGroupsOnce newState remainingGroups

main :: IO ()
main = do
    input <- readInputFile "day9.txt"
    print $ part2 input