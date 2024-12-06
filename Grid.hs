-- Util Functions for Grid-based problems
-- Takes a list of strings and uses those as a grid
module Grid where

import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as Set

data Direction = UP | UP_RIGHT | RIGHT | DOWN_RIGHT | DOWN | DOWN_LEFT | LEFT | UP_LEFT deriving (Eq,Ord,Enum,Show)

type Position = (Int, Int)
type DirectedPosition = (Position, Direction)

orthogonalDirections :: [Direction]
orthogonalDirections =  [UP, RIGHT, DOWN, LEFT]

diagonalDirections :: [Direction]
diagonalDirections =  [UP_RIGHT, DOWN_RIGHT, DOWN_LEFT, UP_LEFT]

dirOffset :: Direction -> Position
dirOffset UP            = (0,    -1)
dirOffset UP_RIGHT      = (1,    -1)
dirOffset RIGHT         = (1,     0)
dirOffset DOWN_RIGHT    = (1,     1)
dirOffset DOWN          = (0,     1)
dirOffset DOWN_LEFT     = (-1,    1)
dirOffset LEFT          = (-1,    0)
dirOffset UP_LEFT       = (-1,   -1)

gridDimensions :: [String] -> Position
gridDimensions grid = (length (head grid), length grid)

-- Get character at location
getAtLocation :: [String] -> Position -> Char
getAtLocation grid (x, y) = (grid !! y) !! x

isValidPosition :: [String] -> Position -> Bool
isValidPosition grid (x, y) =
    let (width, height) = gridDimensions grid
    in x >= 0 && x < width && y >= 0 && y < height

getDirectionSequence :: [String] -> Position -> Position -> Int -> Maybe [Position]
getDirectionSequence grid (x, y) (dx, dy) count =
    let positions = take count [(x + i*dx, y + i*dy) | i <- [1..]]
    in if all (isValidPosition grid) positions
        then Just positions
        else Nothing

getDirectionSequenceUntilBoundary :: [String] -> Position -> Position -> [Position]
getDirectionSequenceUntilBoundary grid (x, y) (dx, dy) =
    takeWhile (isValidPosition grid) [(x + i*dx, y + i*dy) | i <- [1..]]

getAdjacentPositions :: [String] -> Position -> Int -> [[Position]]
getAdjacentPositions grid pos count =
    getOrthogonalPositions grid pos count ++ getDiagonalPositions grid pos count

getOrthogonalPositions :: [String] -> Position -> Int -> [[Position]]
getOrthogonalPositions grid (x, y) count =
    let offsets = map dirOffset orthogonalDirections
        sequences = map (\offset -> getDirectionSequence grid (x, y) offset count) offsets
    in catMaybes sequences

getDiagonalPositions :: [String] -> Position -> Int -> [[Position]]
getDiagonalPositions grid (x, y) count =
    let offsets = map dirOffset diagonalDirections
        sequences = map (\offset -> getDirectionSequence grid (x, y) offset count) offsets
    in catMaybes sequences

getDirectionalPositions :: [String] -> Position -> Direction -> [Position]
getDirectionalPositions grid (x, y) dir =
    getDirectionSequenceUntilBoundary grid (x, y) (dirOffset dir)

getCenterPositions :: [String] -> [Position]
getCenterPositions grid =
    let (width, height) = gridDimensions grid
    in [(x,y) | x <- [1..width-2], y <- [1..height-2]]

getCharacterLocations :: [String] -> Char -> [Position]
getCharacterLocations grid targetChar =
    [(x, y) | y <- [0..height-1],
              x <- [0..width-1],
              (grid !! y) !! x == targetChar]
    where (width, height) = gridDimensions grid

-- Manhattan distance (movement only up/down/left/right)
getManhattanDistance :: Position -> Position -> Int
getManhattanDistance    (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

getPathBetween :: Position -> Position -> [Position]
getPathBetween    (x1, y1) (x2, y2) =
   let dx = signum (x2 - x1)
       dy = signum (y2 - y1)
       path = takeWhile (/= (x2 + dx, y2 + dy)) $ iterate (\(x,y) -> (x + dx, y + dy)) (x1, y1)
   in path

getPositionsFromPath :: [DirectedPosition] -> [Position]
getPositionsFromPath = map fst

visualizePath :: [String] -> [Position] -> IO ()
visualizePath grid positions = do
    let posSet = Set.fromList positions
        visual = [[if (x, y) `Set.member` posSet then '*' else c
                  | (x, c) <- zip [0..] row]
                 | (y, row) <- zip [0..] grid]
    mapM_ putStrLn visual

visualizeDirectedPath :: [String] -> [DirectedPosition] -> IO ()
visualizeDirectedPath grid positions = do
    let dirToArrow UP = "^"
        dirToArrow RIGHT = ">"
        dirToArrow DOWN = "v"
        dirToArrow LEFT = "<"
        dirToArrow _ = "x"  -- Will add diagonals somehow if needed

    let posMap = [(pos, dir) | (pos, dir) <- positions]
    let visual = [concat [maybe [c] dirToArrow (lookup (x, y) posMap)
                         | (x, c) <- zip [0..] row]
                | (y, row) <- zip [0..] grid]
    mapM_ putStrLn visual
