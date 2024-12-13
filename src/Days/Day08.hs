module Days.Day08 (main) where
import TestUtils
import Grid
import Data.List (nub, sort)
import Debug.Trace
import Util (readInputFile)


main :: IO ()
main = do
    input <- readInputFile "day8.txt"
    let grid = lines input
    let ad = findAntenna grid
    let locations = map (\a -> let locs = findAntiNodeLocations a grid in trace ("Antenna " ++ [symbol a] ++ ": " ++ show (length locs)) locs) ad
    let uniqueLocations = nub (concat locations)
    print $ length uniqueLocations

data AntennaData = AntennaData
    { symbol :: Char
    , locations :: [Position]
    } deriving (Show)

findAntenna :: Grid -> [AntennaData]
findAntenna mp =
    let chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
        locs = map (getCharacterLocations mp) chars
    in [AntennaData c l | (c, l) <- zip chars locs, not (null l)]

generateAntiNodes :: Grid -> (Position, Position) -> [Position]
generateAntiNodes grid (p1, p2) = 
    let vector = getVectorBetween p1 p2
        forwardPositions = tail $ takeWhile (isValidPosition grid) $ 
            iterate (addVectorToPoint vector) p2
        backwardPositions = tail $ takeWhile (isValidPosition grid) $ 
            iterate (addVectorToPoint (negateVector vector)) p1
    in forwardPositions ++ backwardPositions
  where
    negateVector (x, y) = (-x, -y)

findAntiNodeLocations :: AntennaData -> Grid -> [Position]
findAntiNodeLocations ad grid = 
    let pairs = [(p1, p2) | p1 <- locations ad, p2 <- locations ad, p1 < p2]
        antiNodes = concatMap (generateAntiNodes grid) pairs
        allPositions = if length (locations ad) >= 2
                      then locations ad ++ antiNodes
                      else antiNodes
        validPos = filter (isValidPosition grid) allPositions
    in validPos

antiNodeGrid :: Grid
antiNodeGrid = [
    "..........",
    "...#......",
    "..........",
    "....a.....",
    "..........",
    ".....a....",
    "..........",
    "......#...",
    "..........",
    ".........."
    ]

threeAntenna :: Grid
threeAntenna = [
    "..........",
    "...#......",
    "#.........",
    "....a.....",
    "........a.",
    ".....a....",
    "..#.......",
    "......#...",
    "..........",
    ".........."
    ]

differentAntenna :: Grid
differentAntenna = [
    "..........",
    "...#......",
    "#.........",
    "....a.....",
    "........a.",
    ".....a....",
    "..#.......",
    "......A...",
    "..........",
    ".........."
    ]

finalExample :: Grid
finalExample = [
    "......#....#",
    "...#....0...",
    "....#0....#.",
    "..#....0....",
    "....0....#..",
    ".#....A.....",
    "...#........",
    "#......#....",
    "........A...",
    ".........A..",
    "..........#.",
    "..........#."
    ]

testHelper :: [String] -> [Position]
testHelper grid = do
    let ad = findAntenna grid
    let res = map (`findAntiNodeLocations` grid) ad
    nub (concat res)

guardLoopTests :: [TestCase [Position]]
guardLoopTests = [
    TestCase {
        testName = "1",
        expected = sort $ getCharacterLocations antiNodeGrid '#',
        actual = sort $ testHelper antiNodeGrid
    },
    TestCase {
        testName = "2",
        expected = sort $ getCharacterLocations threeAntenna '#',
        actual = sort $ testHelper threeAntenna
    },
    TestCase {
        testName = "3",
        expected = sort $ getCharacterLocations differentAntenna '#',
        actual = sort $ testHelper differentAntenna
    },
    TestCase {
        testName = "4",
        expected = sort $ getCharacterLocations finalExample '#',
        actual = sort $ testHelper finalExample
    }
    ]