-- Util Functions for Grid-based problems
-- Takes a list of strings and uses those as a grid
module Grid where

import Data.Maybe (catMaybes)

data Direction = UP | UP_RIGHT | RIGHT | DOWN_RIGHT | DOWN | DOWN_LEFT | LEFT | UP_LEFT deriving (Eq,Ord,Enum)

orthogonalDirections :: [Direction]
orthogonalDirections =  [UP, RIGHT, DOWN, LEFT]

diagonalDirections :: [Direction]
diagonalDirections =  [UP_RIGHT, DOWN_RIGHT, DOWN_LEFT, UP_LEFT]

dirOffset :: Direction -> (Int, Int)
dirOffset UP            = (0,    -1)
dirOffset UP_RIGHT      = (1,    -1)
dirOffset RIGHT         = (1,     0)
dirOffset DOWN_RIGHT    = (1,     1)
dirOffset DOWN          = (0,     1)
dirOffset DOWN_LEFT     = (-1,    1)
dirOffset LEFT          = (-1,    0)
dirOffset UP_LEFT       = (-1,   -1)

gridDimensions :: [String] -> (Int, Int)
gridDimensions grid = (length (head grid), length grid)

-- Get character at location
getAtLocation :: [String] -> (Int, Int) -> Char
getAtLocation grid (x, y) = (grid !! y) !! x

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
    let offsets = map dirOffset orthogonalDirections
        sequences = map (\offset -> getDirectionSequence grid (x, y) offset count) offsets
    in catMaybes sequences

getDiagonalPositions :: [String] -> (Int, Int) -> Int -> [[(Int, Int)]]
getDiagonalPositions grid (x, y) count =
    let offsets = map dirOffset diagonalDirections
        sequences = map (\offset -> getDirectionSequence grid (x, y) offset count) offsets
    in catMaybes sequences

getDirectionalPositions :: [String] -> (Int, Int) -> Direction -> [[(Int, Int)]]
getDirectionalPositions grid (x, y) dir = 
    let offset = dirOffset dir
        maxDist = uncurry max (gridDimensions grid)
        sequence = getDirectionSequence grid (x, y) offset maxDist
    in case sequence of
         Nothing -> []
         Just positions -> [positions]

getCenterPositions :: [String] -> [(Int, Int)]
getCenterPositions grid =
    let (width, height) = gridDimensions grid
    in [(x,y) | x <- [1..width-2], y <- [1..height-2]]

getCharacterLocations :: [String] -> Char -> [(Int, Int)]
getCharacterLocations grid targetChar =
    [(x, y) | y <- [0..height-1],
              x <- [0..width-1],
              (grid !! y) !! x == targetChar]
    where (width, height) = gridDimensions grid

-- Manhattan distance (movement only up/down/left/right)
getManhattanDistance :: (Int, Int) -> (Int, Int) -> Int
getManhattanDistance    (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)
