import Data.Maybe (catMaybes)

main :: IO ()
main = do
    wordsearch <- readFile "day4.txt"
    let grid = lines wordsearch
    let startLocations = getStartingLocations grid 'X'
    let wordsFromLocations = [findWordsFromPositionPart1 grid "XMAS" x | x <- startLocations]
    let total = sum wordsFromLocations
    print "Part 1 total:"
    print total
    print "Part 2 total:"
    print $ length $ findDiagonalWords grid "MAS"

gridDimensions :: [String] -> (Int, Int)
gridDimensions grid = (length (head grid), length grid)

getLetter :: [String] -> (Int, Int) -> Char
getLetter grid (x, y) = (grid !! y) !! x

getWord :: [String] -> (Int, Int) -> [(Int, Int)] -> String
getWord grid start sequence =
    getLetter grid start : [getLetter grid pos | pos <- sequence]

isValidPosition :: [String] -> (Int, Int) -> Bool
isValidPosition grid (x, y) =
    let (width, height) = gridDimensions grid
    in x >= 0 && x < width && y >= 0 && y < height

getDirectionSequence :: [String] -> (Int, Int) -> (Int, Int) -> Int -> Maybe [(Int, Int)]
getDirectionSequence grid (x, y) (dx, dy) count =
    let positions = take count [(x + i*dx, y + i*dy) | i <- [1..]]
    in if all (isValidPosition grid) positions
        then Just positions
        else Nothing

getAdjacentPositions :: [String] -> (Int, Int) -> Int -> [[(Int, Int)]]
getAdjacentPositions grid pos count =
    getOrthogonalPositions grid pos count ++ getDiagonalPositions grid pos count

getOrthogonalPositions :: [String] -> (Int, Int) -> Int -> [[(Int, Int)]]
getOrthogonalPositions grid (x, y) count =
    let offsets = [(-1,0), (1,0), (0,-1), (0,1)]
        sequences = map (\offset -> getDirectionSequence grid (x, y) offset count) offsets
    in catMaybes sequences

getDiagonalPositions :: [String] -> (Int, Int) -> Int -> [[(Int, Int)]]
getDiagonalPositions grid (x, y) count =
    let offsets = [(-1,-1), (-1,1), (1,-1), (1,1)]
        sequences = map (\offset -> getDirectionSequence grid (x, y) offset count) offsets
    in catMaybes sequences

getDiagonalWords :: [String] -> (Int, Int) -> [(String, String)]
getDiagonalWords grid (x, y) =
    [(word1, word2) | all (isValidPosition grid) diagonal1 && all (isValidPosition grid) diagonal2]
    where
        diagonal1 = [(x-1,y-1), (x,y), (x+1,y+1)]
        diagonal2 = [(x-1,y+1), (x,y), (x+1,y-1)]
        word1 = map (getLetter grid) diagonal1
        word2 = map (getLetter grid) diagonal2

getStartingLocations :: [String] -> Char -> [(Int, Int)]
getStartingLocations grid targetChar =
    [(x, y) | y <- [0..height-1],
              x <- [0..width-1],
              (grid !! y) !! x == targetChar]
    where (width, height) = gridDimensions grid

findWordsFromPositionPart1 :: [String] -> String -> (Int, Int) -> Int
findWordsFromPositionPart1 grid word start =
    let firstChar = getLetter grid start
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

getCenterPositions :: [String] -> [(Int, Int)]
getCenterPositions grid =
    let (width, height) = gridDimensions grid
    in [(x,y) | x <- [1..width-2], y <- [1..height-2]]

findDiagonalWords :: [String] -> String -> [(Int, Int)]
findDiagonalWords grid word =
    filter (hasWordInBothDiagonals grid word) (getCenterPositions grid)