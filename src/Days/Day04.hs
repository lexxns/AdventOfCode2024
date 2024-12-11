module Days.Day04 (main) where

import Util (readInputFile)
import Grid
    ( gridDimensions,
      getAtLocation,
      isValidPosition,
      getAdjacentPositions,
      getCenterPositions,
      getCharacterLocations )

main :: IO ()
main = do
    wordsearch <- readInputFile "day4.txt"
    let grid = lines wordsearch
    let startLocations = getCharacterLocations grid 'X'
    let wordsFromLocations = [findWordsFromPositionPart1 grid "XMAS" x | x <- startLocations]
    let total = sum wordsFromLocations
    print "Part 1:"
    print total
    print "Part 2:"
    print $ length $ findDiagonalWords grid "MAS"

getWord :: [String] -> (Int, Int) -> [(Int, Int)] -> String
getWord grid start sequence =
    getAtLocation grid start : [getAtLocation grid pos | pos <- sequence]

getDiagonalWords :: [String] -> (Int, Int) -> [(String, String)]
getDiagonalWords grid (x, y) =
    [(word1, word2) | all (isValidPosition grid) diagonal1 && all (isValidPosition grid) diagonal2]
    where
        diagonal1 = [(x-1,y-1), (x,y), (x+1,y+1)]
        diagonal2 = [(x-1,y+1), (x,y), (x+1,y-1)]
        word1 = map (getAtLocation grid) diagonal1
        word2 = map (getAtLocation grid) diagonal2

findWordsFromPositionPart1 :: [String] -> String -> (Int, Int) -> Int
findWordsFromPositionPart1 grid word start =
    let firstChar = getAtLocation grid start
        sequences = getAdjacentPositions grid start 3
    in if firstChar /= head word
       then 0
       else length $ filter (\seq -> word == getWord grid start seq) sequences

matchesWord :: String -> String -> Bool
matchesWord target word = word == target || reverse word == target

hasWordInBothDiagonals :: [String] -> String -> (Int, Int) -> Bool
hasWordInBothDiagonals grid word pos =
    case getDiagonalWords grid pos of
        [(word1, word2)] -> matchesWord word word1 && matchesWord word word2
        _ -> False

findDiagonalWords :: [String] -> String -> [(Int, Int)]
findDiagonalWords grid word =
    filter (hasWordInBothDiagonals grid word) (getCenterPositions grid)