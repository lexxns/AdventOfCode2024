module Main where

import Data.List (tails)

isValidDiff :: Int -> Int -> Bool
isValidDiff x y = abs (y - x) <= 3

isIncreasing :: [Int] -> Bool
isIncreasing [] = True
isIncreasing [_] = True
isIncreasing (x:y:xs) = x < y && isValidDiff x y && isIncreasing (y:xs)

isDecreasing :: [Int] -> Bool
isDecreasing [] = True
isDecreasing [_] = True
isDecreasing (x:y:xs) = x > y && isValidDiff x y && isDecreasing (y:xs)

isValidMonotonic :: [Int] -> Bool
isValidMonotonic xs = isIncreasing xs || isDecreasing xs

removeAt :: Int -> [a] -> [a]
removeAt i xs = take i xs ++ drop (i + 1) xs

canBecomeValidByRemovingOne :: [Int] -> Bool
canBecomeValidByRemovingOne xs =
    any (isValidMonotonic . (`removeAt` xs)) [0..length xs - 1]

isSafeList :: [Int] -> Bool
isSafeList xs = isValidMonotonic xs || canBecomeValidByRemovingOne xs

readNumberFile :: FilePath -> IO [[Int]]
readNumberFile path = do
    contents <- readFile path
    return $ map (map read . words) $ lines contents

main :: IO ()
main = do
    numbers <- readNumberFile "day2.txt"
    let safeNumbers = filter isSafeList numbers
    putStrLn "Safe lists:"
    print $ length safeNumbers
