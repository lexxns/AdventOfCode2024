-- Util Functions for Grid-based problems
-- Takes a list of strings and uses those as a grid
module Grid where

import Data.Maybe (catMaybes, listToMaybe)
import qualified Data.Set as Set

data Direction = UP | UP_RIGHT | RIGHT | DOWN_RIGHT | DOWN | DOWN_LEFT | LEFT | UP_LEFT deriving (Eq,Ord,Enum,Show)

type Grid = [String]
type Position = (Int, Int)
type Vector = (Int, Int)
type Edge = (Position, Position)
type DirectedPosition = (Position, Direction)
type Region = Set.Set Position

isInRegion :: Position -> Region -> Bool
isInRegion = Set.member

adjacent :: Position -> [Position]
adjacent (x, y) = [(x, y+1), (x+1, y), (x, y-1), (x-1, y)]

countSides :: Region -> Int
countSides region = sum $ map (countSidesForPos region) (Set.toList region)

countSidesForPos :: Region -> Position -> Int
countSidesForPos region pos = 
    let adjPositions = adjacent pos
        nonMemberAdj = filter (not . (`Set.member` region)) adjPositions
    in length nonMemberAdj

getEdgeSegments :: Region -> Position -> [Edge]
getEdgeSegments region (x, y) =
    let neighbors = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
        -- Only create border segments where there's a transition from region to non-region
        isExternalNeighbor pos = not (Set.member pos region)
    in [((x, y), neighbor) | neighbor <- neighbors, isExternalNeighbor neighbor]

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

positionInDirection :: Position -> Direction -> Int -> Position
positionInDirection (x, y) dir count = 
    let (offsetX, offsetY) = dirOffset dir
    in (x + offsetX * count, y + offsetY * count)

gridDimensions :: Grid -> Position
gridDimensions grid = (length (head grid), length grid)

gridPositions :: Int -> Int -> [Position]
gridPositions width height = [(x, y) | y <- [0..height-1], x <- [0..width-1]]

-- Get character at location
getAtLocation :: Grid -> Position -> Char
getAtLocation grid (x, y) = (grid !! y) !! x

setAtLocation :: Grid -> Position -> Char -> Grid
setAtLocation grid (x, y) ch =
    let row = grid !! y
        newRow = take x row ++ [ch] ++ drop (x + 1) row
    in take y grid ++ [newRow] ++ drop (y + 1) grid

setAtLocations :: Grid -> [Position] -> Char -> Grid
setAtLocations grid positions ch =
    foldl (\g pos -> setAtLocation g pos ch) grid positions

isValidPosition :: Grid -> Position -> Bool
isValidPosition grid (x, y) =
    let (width, height) = gridDimensions grid
    in x >= 0 && x < width && y >= 0 && y < height

getDirectionSequence :: Grid -> Position -> Position -> Int -> Maybe [Position]
getDirectionSequence grid (x, y) (dx, dy) count =
    let positions = take count [(x + i*dx, y + i*dy) | i <- [1..]]
    in if all (isValidPosition grid) positions
        then Just positions
        else Nothing

getDirectionSequenceUntilBoundary :: Grid -> Position -> Position -> [Position]
getDirectionSequenceUntilBoundary grid (x, y) (dx, dy) =
    takeWhile (isValidPosition grid) [(x + i*dx, y + i*dy) | i <- [1..]]

getAdjacentPositions :: Grid -> Position -> Int -> [[Position]]
getAdjacentPositions grid pos count =
    getOrthogonalPositionsWithCount grid pos count ++ getDiagonalPositionsWithCount grid pos count

getOrthogonalPositions :: Grid -> Position -> [[Position]]
getOrthogonalPositions grid (x, y) = getOrthogonalPositionsWithCount grid (x, y) 1

getOrthogonalPositionsWithCount :: Grid -> Position -> Int -> [[Position]]
getOrthogonalPositionsWithCount grid (x, y) count =
    let offsets = map dirOffset orthogonalDirections
        sequences = map (\offset -> getDirectionSequence grid (x, y) offset count) offsets
    in catMaybes sequences

getDiagonalPositions :: Grid -> Position -> [[Position]]
getDiagonalPositions grid (x, y) = getDiagonalPositionsWithCount grid (x, y) 1

getDiagonalPositionsWithCount :: Grid -> Position -> Int -> [[Position]]
getDiagonalPositionsWithCount grid (x, y) count =
    let offsets = map dirOffset diagonalDirections
        sequences = map (\offset -> getDirectionSequence grid (x, y) offset count) offsets
    in catMaybes sequences

getDirectionalPositions :: Grid -> Position -> Direction -> [Position]
getDirectionalPositions grid (x, y) dir =
    getDirectionSequenceUntilBoundary grid (x, y) (dirOffset dir)

getCenterPositions :: Grid -> [Position]
getCenterPositions grid =
    let (width, height) = gridDimensions grid
    in [(x,y) | x <- [1..width-2], y <- [1..height-2]]

getCharacterLocations :: Grid -> Char -> [Position]
getCharacterLocations grid targetChar =
    [(x, y) | x <- [0..width-1],
              y <- [0..height-1],
              (grid !! y) !! x == targetChar]
    where (width, height) = gridDimensions grid

getCharacterLocation :: Grid -> Char -> Maybe Position
getCharacterLocation grid char = listToMaybe (getCharacterLocations grid char)

-- Manhattan distance (movement only up/down/left/right)
getManhattanDistance :: Position -> Position -> Int
getManhattanDistance    (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

getVectorBetween :: Position -> Position -> Vector
getVectorBetween    (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

addVectorToPoint :: Position -> Vector -> Position
addVectorToPoint    (x1, y1) (d1, d2) = (x1 + d1, y1 + d2)

addVectorToPointWrapped :: Grid -> Position -> Vector -> Position
addVectorToPointWrapped grid (x, y) (dx, dy) =
    let (width, height) = gridDimensions grid
        newX = (x + dx) `mod` width
        newY = (y + dy) `mod` height
    in (newX, newY)

getPathBetween :: Position -> Position -> [Position]
getPathBetween    (x1, y1) (x2, y2) =
   let dx = signum (x2 - x1)
       dy = signum (y2 - y1)
       path = takeWhile (/= (x2 + dx, y2 + dy)) $ iterate (\(x,y) -> (x + dx, y + dy)) (x1, y1)
   in path

getPositionsFromPath :: [DirectedPosition] -> [Position]
getPositionsFromPath = map fst

isContainedWithin :: Region -> Region -> Bool
isContainedWithin = Set.isSubsetOf

visualizePath :: Grid -> [Position] -> IO ()
visualizePath grid positions = do
    let posSet = Set.fromList positions
        visual = [[if (x, y) `Set.member` posSet then '*' else c
                  | (x, c) <- zip [0..] row]
                 | (y, row) <- zip [0..] grid]
    mapM_ putStrLn visual

visualizeDirectedPath :: Grid -> [DirectedPosition] -> IO ()
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

visualizeGrid :: Grid -> IO ()
visualizeGrid = mapM_ putStrLn