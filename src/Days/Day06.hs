{-# LANGUAGE TupleSections #-}
module Days.Day06 (main) where

import Grid (getDirectionalPositions, getCharacterLocations, getAtLocation, orthogonalDirections, Position, DirectedPosition, getPositionsFromPath)
import Data.List (find)
import qualified Data.Set as Set
import Util (readInputFile)
import TestUtils

main :: IO ()
main = do
    input <- readInputFile "day6.txt"
    let grid = lines input
    let guardLocation = head $ getCharacterLocations grid '^'
    let pathResult = followGuardPath grid guardLocation
    let positions = getPositionsFromPath $ path pathResult
    let uniqueLocations = Set.fromList positions :: Set.Set Position
    print "Part 1:"
    print $ length uniqueLocations
    let possibleLoopPositions = findLoopPositions grid guardLocation $ path pathResult
    print "Part 2:"
    print $ length possibleLoopPositions

data PathResult = PathResult {
    path :: [DirectedPosition],
    foundLoop :: Bool
} deriving (Show)

followGuardPath :: [String] -> Position -> PathResult
followGuardPath grid startPos = move startPos (cycle orthogonalDirections) [] Set.empty
  where
    move pos (dir:restDirs) acc seen
      | Set.member (pos, dir) seen = PathResult acc True  -- Found a loop
      | otherwise = 
          let positions = pos : getDirectionalPositions grid pos dir
              directedPositions = map (, dir) positions
              obstaclePos = find (\p -> getAtLocation grid p == '#') positions
              newSeen = Set.insert (pos, dir) seen
          in case obstaclePos of
            Nothing -> PathResult (acc ++ directedPositions) False
            Just obstacle ->
              let path = takeWhile (/= obstacle) positions
                  directedPath = map (, dir) path
                  newPos = if null path then pos else last path
              in move newPos restDirs (acc ++ directedPath) newSeen


hasLoop :: [DirectedPosition] -> Bool
hasLoop path = 
    let uniqueStates = Set.fromList path
    in length uniqueStates < length path

-- Add an obstacle to the grid at the given position
addObstacle :: [String] -> Position -> [String]
addObstacle grid (x, y) =
    let (before, row:after) = splitAt y grid
        (beforeCol, _:afterCol) = splitAt x row
        newRow = beforeCol ++ "#" ++ afterCol
    in before ++ [newRow] ++ after

findLoopPositions :: [String] -> Position -> [DirectedPosition] -> [Position]
findLoopPositions grid startPos guardPath =
        let pathPositions = Set.fromList $ getPositionsFromPath guardPath
            isLoopPosition pos =
                pos `Set.member` pathPositions &&  -- Only test positions on the path
                let modifiedGrid = addObstacle grid pos
                    newPath = path $ followGuardPath modifiedGrid startPos
                in hasLoop newPath
    in filter isLoopPosition (Set.toList pathPositions)

loopGrid :: [String]
loopGrid = [
    ".#...",
    ".^..#",  -- Should loop
    ".....",
    ".....",
    "#..#."
    ]

noLoopGrid :: [String]
noLoopGrid = [
    ".#...",
    ".^..#",  -- Guard starts up but blocked on three sides
    ".....",
    ".....",
    "#...."
    ]

guardLoopTests :: [TestCase Bool]
guardLoopTests = [
    TestCase {
        testName = "Loop",
        expected = True,
        actual = foundLoop $ followGuardPath loopGrid (1,1)
    },
    TestCase {
        testName = "No Loop",
        expected = False,
        actual = foundLoop $ followGuardPath loopGrid (1,1)
    }
    ]
