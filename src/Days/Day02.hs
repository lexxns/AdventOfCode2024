module Days.Day02 (main) where

import Util (readInputFileLines)

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

isSafeListPart1 :: [Int] -> Bool
isSafeListPart1 = isValidMonotonic

isSafeListPart2 :: [Int] -> Bool
isSafeListPart2 xs = isValidMonotonic xs || canBecomeValidByRemovingOne xs

readNumberFile :: [String] -> IO [[Int]]
readNumberFile input = do
    return $ map (map read . words) input

main :: IO ()
main = do
    numbers <- readNumberFile $ readInputFileLines "day2.txt"
    let p1 = filter isSafeListPart1 numbers
    putStrLn "Part 1:"
    print $ length p1
    let p2 = filter isSafeListPart2 numbers
    putStrLn "Part 2:"
    print $ length p2
