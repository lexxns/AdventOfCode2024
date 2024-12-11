module Days.Day03 (main) where

import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import Util (readInputFile)

main :: IO ()
main = do
    code <- readInputFile "day3.txt"
    let p1pairs = countPart1 code
    let p1totals = multiplyTuples p1pairs
    let p1total = sum p1totals
    print "Part 1:"
    print p1total
    let p2pairs = countPart2 True code
    let p2totals = multiplyTuples p2pairs
    let p2total = sum p2totals
    print "Part 2:"
    print p2total


multiplyTuples :: [(Int, Int)] -> [Int]
multiplyTuples tuples = [x * y | (x,y) <- tuples]

dropTillNextMul :: String -> String
dropTillNextMul = dropWhile (/= 'm')

dropTillNextInstruction :: String -> String
dropTillNextInstruction = dropWhile (\c -> c /= 'm' && c /= 'd')

extractNumbers :: String -> Maybe (Int, Int)
extractNumbers str = do
    (x, rest) <- case reads str of
        [(n, r)] -> Just (n, r)
        _ -> Nothing

    if null rest || head rest /= ',' 
        then Nothing
        else do
            (y, rest') <- case reads (tail rest) of
                [(n, r)] -> Just (n, r)
                _ -> Nothing

            if null rest' || head rest' /= ')'
                then Nothing
                else Just (x, y)

countPart1 :: String -> [(Int, Int)]
countPart1 "" = []
countPart1 input@(c:cs)
    | length input < 4 = []
    | "mul(" `isPrefixOf` input = 
        case extractNumbers (drop 4 input) of
            Just a -> a : countPart1 (dropTillNextMul cs)
            Nothing -> countPart1 (dropTillNextMul cs)
    | otherwise = countPart1 cs

countPart2 :: Bool -> String -> [(Int, Int)]
countPart2 _ "" = []
countPart2 state input@(c:cs)
    | length input < 4 = []
    | "do()" `isPrefixOf` input = 
        countPart2 True (drop 4 input)
    | "don't()" `isPrefixOf` input = 
        countPart2 False (drop 6 input)
    | "mul(" `isPrefixOf` input && state =
        case extractNumbers (drop 4 input) of
            Just (x, y) -> (x, y) : countPart2 state (dropTillNextInstruction cs)
            Nothing -> countPart2 state (dropTillNextInstruction cs)
    | "mul(" `isPrefixOf` input && not state =
        countPart2 state (dropTillNextInstruction cs)
    | otherwise = countPart2 state cs

