module Days.Day12 (main) where

import Util (readInputFile)
import Grid (getAtLocation, gridDimensions, gridPositions, Position, Grid, getOrthogonalPositions)
import qualified Data.Set as Set
import Control.Monad.State
import TestUtils

data Region = Region
    { area :: Int
    , perimeter :: Int
    , plant :: Char
    , price :: Int
    , positions :: [Position]
    , sides :: Int
    } deriving (Show)

type Cache = State (Set.Set Position)

findBoundaryPositions :: Grid -> [Position] -> [Position]
findBoundaryPositions grid positions = 
    let posSet = Set.fromList positions
        (width, height) = gridDimensions grid
        isBoundary (x,y) = any (\pos -> 
            let (nx,ny) = pos
            in nx < 0 || nx >= width || ny < 0 || ny >= height || 
               not (Set.member pos posSet)) [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
    in filter isBoundary positions

findRegion :: Grid -> Position -> Region
findRegion grid startPos = 
    let startPlant = getAtLocation grid startPos
        (positions, visited) = runState (exploreRegion grid startPlant startPos) Set.empty
        regionArea = length positions
        regionPerimeter = calculatePerimeter grid positions
    in Region 
        { area = regionArea
        , perimeter = regionPerimeter
        , plant = startPlant
        , price = regionArea * regionPerimeter
        , positions = positions
        }

exploreRegion :: Grid -> Char -> Position -> Cache [Position]
exploreRegion grid targetPlant pos = do
    visited <- get
    if Set.member pos visited
        then return []
        else do
            put (Set.insert pos visited)
            if getAtLocation grid pos /= targetPlant
                then return []
                else do
                    let neighbors = concat $ getOrthogonalPositions grid pos 1
                    neighborPositions <- concat <$> mapM (exploreRegion grid targetPlant) neighbors
                    return (pos : neighborPositions)

calculatePerimeter :: Grid -> [Position] -> Int
calculatePerimeter grid positions = 
    let (width, height) = gridDimensions grid
        posSet = Set.fromList positions
        isInRegion pos = Set.member pos posSet
        isOutOfBounds (x, y) = x < 0 || x >= width || y < 0 || y >= height
        getChar pos@(x, y)
            | isOutOfBounds pos = Nothing
            | otherwise = Just (getAtLocation grid pos)
        countEdges pos@(x, y) = 
            let neighbors = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
                isEdge npos = isOutOfBounds npos || 
                             not (isInRegion npos)
            in length $ filter isEdge neighbors
    in sum [countEdges pos | pos <- positions]

processPositions :: Grid -> [Position] -> Cache [Region]
processPositions grid [] = return []
processPositions grid (pos:rest) = do
    visited <- get
    if Set.member pos visited
        then processPositions grid rest
        else do
            let region = findRegion grid pos
            modify (Set.union (Set.fromList $ positions region))
            remaining <- processPositions grid rest
            return (region : remaining)
    where (width, height) = gridDimensions grid

main :: IO ()
main = do
    input <- readInputFile "day12.txt"
    let grid = lines input
    let (width, height) = gridDimensions grid
    let positions = gridPositions width height
    let findAllRegions = evalState (processPositions grid positions) Set.empty
    let total = sum [price region | region <- findAllRegions]
    print "Part 1:"
    print total

firstExample :: Grid
firstExample = [
    "AAAA",
    "BBCD",
    "BBCC",
    "EEEC"
    ]

secondExample :: Grid
secondExample = [
    "OOOOO",
    "OXOXO",
    "OOOOO",
    "OXOXO",
    "OOOOO"
    ]

finalExample :: Grid
finalExample = [
    "RRRRIICCFF",
    "RRRRIICCCF",
    "VVRRRCCFFF",
    "VVRCCCJFFF",
    "VVVVCJJCFE",
    "VVIVCCJJEE",
    "VVIIICJJEE",
    "MIIIIIJJEE",
    "MIIISIJEEE",
    "MMMISSJEEE"
    ]

priceTests :: [TestCase Int]
priceTests = [
    TestCase {
        testName = "1",
        expected = 140,
        actual = testHelper firstExample
    },
    TestCase {
        testName = "2",
        expected = 772,
        actual = testHelper secondExample
    },
    TestCase {
        testName = "3",
        expected = 1930,
        actual = testHelper finalExample
    }
    ]

testHelper :: Grid -> Int
testHelper grid = do
    let (width, height) = gridDimensions grid
    let positions = gridPositions width height
    let findAllRegions = evalState (processPositions grid positions) Set.empty
    sum [price region | region <- findAllRegions]