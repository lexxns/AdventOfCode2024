module Days.Day12 (main) where

import Util (readInputFile)
import Grid (getAtLocation, gridDimensions, gridPositions, Position, Grid, getOrthogonalPositions, countSides, Region)
import qualified Data.Set as Set
import Control.Monad.State
import TestUtils

data PlantPlot = PlantPlot
    { area :: Int
    , perimeter :: Int
    , plant :: Char
    , price :: Int
    , discountPrice :: Int
    , positions :: [Position]
    , region :: Region
    } deriving (Show)

type Cache = State (Set.Set Position)

findRegion :: Grid -> Position -> PlantPlot
findRegion grid startPos = 
    let startPlant = getAtLocation grid startPos
        (gp, r) = runState (exploreRegion grid startPlant startPos) Set.empty 
        regionArea = length gp
        regionPerimeter = calculatePerimeter grid gp
        regionSides = countSides r
    in PlantPlot 
        { area = regionArea
        , perimeter = regionPerimeter
        , plant = startPlant
        , price = regionArea * regionPerimeter
        , discountPrice = regionArea * regionSides
        , positions = gp
        , region = r
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
                    let neighbors = concat $ getOrthogonalPositions grid pos
                    neighborPositions <- concat <$> mapM (exploreRegion grid targetPlant) neighbors
                    return (pos : neighborPositions)

calculatePerimeter :: Grid -> [Position] -> Int
calculatePerimeter grid gp = 
    let (width, height) = gridDimensions grid
        posSet = Set.fromList gp
        isInRegion pos = Set.member pos posSet
        isOutOfBounds (x, y) = x < 0 || x >= width || y < 0 || y >= height
        countEdges (x, y) = 
            let neighbors = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
                isEdge npos = isOutOfBounds npos || 
                             not (isInRegion npos)
            in length $ filter isEdge neighbors
    in sum [countEdges pos | pos <- gp]

processPositions :: Grid -> [Position] -> Cache [PlantPlot]
processPositions _ [] = return []
processPositions grid (pos:rest) = do
    visited <- get
    if Set.member pos visited
        then processPositions grid rest
        else do
            let r = findRegion grid pos
            modify (Set.union (Set.fromList $ positions r))
            remaining <- processPositions grid rest
            return (r : remaining)

main :: IO ()
main = do
    runTests priceTests

    input <- readInputFile "day12.txt"
    let grid = lines input
    let (width, height) = gridDimensions grid
    let gp = gridPositions width height
    let findAllRegions = evalState (processPositions grid gp) Set.empty
    let p1 = sum [price r | r <- findAllRegions]
    print "Part 1:"
    print p1
    let p2 = sum [discountPrice r | r <- findAllRegions]
    print "Part 2:"
    print p2

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
        actual = testHelperPart1 firstExample
    },
    TestCase {
        testName = "2",
        expected = 772,
        actual = testHelperPart1 secondExample
    },
    TestCase {
        testName = "3",
        expected = 1930,
        actual = testHelperPart1 finalExample
    },
    TestCase {
        testName = "4",
        expected = 80,
        actual = testHelperPart2 firstExample
    }
    ]

testHelperPart1 :: Grid -> Int
testHelperPart1 grid = do
    let (width, height) = gridDimensions grid
    let gp = gridPositions width height
    let findAllRegions = evalState (processPositions grid gp) Set.empty
    sum [price r | r <- findAllRegions]
    
testHelperPart2 :: Grid -> Int
testHelperPart2 grid = 
    let (width, height) = gridDimensions grid
        gp = gridPositions width height
        findAllRegions = evalState (processPositions grid gp) Set.empty
    in length findAllRegions

-- Separate debug function that performs IO
debugPlantPlots :: Grid -> IO ()
debugPlantPlots grid = do
    let (width, height) = gridDimensions grid
        gp = gridPositions width height
        findAllRegions = evalState (processPositions grid gp) Set.empty
    mapM_ testGetCounts findAllRegions

testGetCounts :: PlantPlot -> IO ()
testGetCounts pp = do
    putStrLn $ "Found Region: " ++ [plant pp]
    putStrLn $ "Region: " ++ show (region pp)
    putStrLn $ "Sides: " ++ show (countSides (region pp))

testRegion :: Region
testRegion = Set.fromList [(0,0), (1,0), (1,1)]

testCountSides = do
    -- Test with L-shaped region
    putStrLn $ "L-shaped region sides: " ++ show (countSides testRegion)  -- Should be 8
    
    -- Test with single square
    let singleSquare = Set.singleton (0,0)
    putStrLn $ "Single square sides: " ++ show (countSides singleSquare)  -- Should be 4
    
    -- Test with 2x2 square
    let square2x2 = Set.fromList [(0,0), (0,1), (1,0), (1,1)]
    putStrLn $ "2x2 square sides: " ++ show (countSides square2x2)  -- Should be 8