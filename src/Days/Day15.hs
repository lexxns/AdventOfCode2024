{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Days.Day15 (main) where
import TestUtils
import Grid
import Debug.Trace (trace)
import Util (readInputFile)
import qualified Data.Set as Set
import Numeric (showHex)


runTestCase :: Grid -> String -> Grid
runTestCase grd input =
    let dirs = parseDirections input
    in foldl makeMove grd dirs

main :: IO ()
main = do
   input <- readInputFile "day15.txt"
   let moves = concat $ lines input
   let finalGrid = runTestCase day15Grid moves
   let score = sumBoxes finalGrid
   print "Part 1:"
   print score

sumBoxes :: Grid -> Integer
sumBoxes grd =
    let boxLocations = getCharacterLocations grd 'O'
        scores = map boxScore boxLocations
    in sum scores
    where
        boxScore (x, y) = fromIntegral (y * 100 + x)


parseDirections :: String -> [Direction]
parseDirections = map dirFromArrow where
    dirFromArrow i
        | i == '<' = LEFT
        | i == '^' = UP
        | i == '>' = RIGHT
        | i == 'v' = DOWN

makeMove :: Grid -> Direction -> Grid
makeMove grid dir =
    case getCharacterLocation grid '@' of
        Nothing -> grid
        Just startPos ->
            let newPos = positionInDirection startPos dir 1
                targetChar = getAtLocation grid newPos
            in case targetChar of
                '.' -> replaceAtLocation
                    (replaceAtLocation grid newPos '@')
                    startPos '.'
                b | b `elem` ['O', '[', ']'] -> case canPushBoxes grid dir newPos of
                    Nothing -> grid  -- Can't push the boxes
                    Just endPos -> pushBoxes grid dir startPos newPos endPos
                _ -> grid

canPushBoxes :: Grid -> Direction -> Position -> Maybe Position
canPushBoxes grid dir (x, y) =
    let nextPos = positionInDirection (x, y) dir 1
        char = getAtLocation grid nextPos
    in case char of
        '.' -> Just (x, y)
        'O' -> canPushBoxes grid dir nextPos
        '[' -> canPushBoxes grid dir nextPos >>= \_ ->
               let (nextX, nextY) = nextPos in
               canPushBoxes grid dir (nextX + 1, nextY) >>= \_ ->
               Just (x, y)
        ']' -> canPushBoxes grid dir nextPos >>= \_ ->
               let (nextX, nextY) = nextPos in
               canPushBoxes grid dir (nextX - 1, nextY) >>= \_ ->
               Just (x, y)
        _   -> Nothing

-- Get all positions containing boxes from start to end (inclusive)
getBoxPositions :: Position -> Position -> Direction -> [Position]
getBoxPositions start end dir =
    takeWhile (/= positionInDirection end dir 1) $
    iterate (\pos -> positionInDirection pos dir 1) start

pushBoxes :: Grid -> Direction -> Position -> Position -> Position -> Grid
pushBoxes grid dir playerPos firstBoxPos endPos =
    let boxPositions = getBoxPositions firstBoxPos endPos dir
        gridWithBoxesMoved = foldl moveBox grid (reverse boxPositions)
        finalGrid = replaceAtLocation 
            (replaceAtLocation gridWithBoxesMoved firstBoxPos '@') 
            playerPos '.'
    in finalGrid
    where
        moveBox g pos = 
            let newPos = positionInDirection pos dir 1
            in replaceAtLocation (replaceAtLocation g newPos 'O') pos '.'

replaceAtLocation :: Grid -> Position -> Char -> Grid
replaceAtLocation grid (x, y) newChar =
    let row = grid !! y
        newRow = take x row ++ [newChar] ++ drop (x + 1) row
    in take y grid ++ [newRow] ++ drop (y + 1) grid


exampleGrid :: Grid
exampleGrid = [
    "########",
    "#..O.O.#",
    "##@.O..#",
    "#...O..#",
    "#.#.O..#",
    "#...O..#",
    "#......#",
    "########"
    ]
expectedGrid = [
    "########",
    "#....OO#",
    "##.....#",
    "#.....O#",
    "#.#O@..#",
    "#...O..#",
    "#...O..#",
    "########"
    ]

largerExample = [
    "##########",
    "#..O..O.O#",
    "#......O.#",
    "#.OO..O.O#",
    "#..O@..O.#",
    "#O#..O...#",
    "#O..O..O.#",
    "#.OO.O.OO#",
    "#....O...#",
    "##########"
    ]

exampleInput = "<^^>>>vv<v>>v<<"

largerExampleInput = "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"

testHelper :: Grid -> String -> Grid
testHelper grd input =
    let dirs = parseDirections input
    in foldl makeMove grd dirs

movementTests :: [TestCase Grid]
movementTests = [
    TestCase {
        testName = "1",
        expected = expectedGrid,
        actual = testHelper exampleGrid exampleInput
    }
    ]

countTest :: [TestCase Integer]
countTest = [
    TestCase {
        testName = "2",
        expected = 10092,
        actual = sumBoxes $ testHelper largerExample largerExampleInput
    },
    TestCase {
        testName = "2",
        expected = 2028,
        actual = sumBoxes $ testHelper exampleGrid exampleInput
    }
    ]

day15Grid :: Grid
day15Grid = [
    "##################################################",
    "##OO............#..........O.O..O.O.....O..O..O..#",
    "#O...#.O.O.O....#OO.#O....#.........O.#..........#",
    "#O....O..O.OO.OOO.#.....O#.....#..#O.OO..........#",
    "#.O#OOO...O............O##......#..O..O......O..##",
    "#......#....#...OO....O.#OO..O.OO.OO.O#........#.#",
    "#...O...##.....OO...O..#...O....O....O.....O...O.#",
    "##.O.#...##....O..............#....O....O.O..O#.O#",
    "#.OO#...O........O......#.#O...OO.OO...O.O.#....##",
    "#O....O...O.O.O.#.#OO.O#.O....#..O..O...#....OO..#",
    "#O.O..OO..##.....#.#O..O.O.OOO.....O..#.O..#.....#",
    "#.....OO..O...O.OO..#......O...O...O..O.#........#",
    "#..O..O#.O.O.O..#.O.O..OO....#....O#.....O.OO..O.#",
    "#..#O#..O..O.....#.#O...O..OO#.OO..OO...........O#",
    "#..............O#O..#OO..OO...OOO.....O#..#...O.##",
    "#....OO..#...O..#..O.#..OOO........#OO.O.O..O....#",
    "#O#.#.O.O.OO..O.....OO..##....O..O........#.O....#",
    "#.O.O.O.O#O..#.....O#.OO.#O.....O.O......O..O....#",
    "#O.....O#O...O#....OO.OO.......O.O...O.......O...#",
    "#....OO....O.O..O.#.......OOOO...O.O..OO....OO...#",
    "##.....O....O..O.O..OOOO.....#...OO.....O#...#..O#",
    "#O.#...O...O.O.O.OO.O...#....O..O.OO..O...O#..O.O#",
    "#....O.#O.OOOOO..OO...OOO.#OO.O...O...O...O..O...#",
    "##O.....#O.....O....O..O.O...O...O.O..OO.O.O...O.#",
    "#......#.O#.O...#....OO.@#.....##..#.OO#....#....#",
    "#OOO...O..OO.........O.#.O....O#...O.OO....#O...O#",
    "#...OO..O..O#...O..O.#O.O.O#............O......O.#",
    "#OO.O.OO..OO.#....O....O.....OO....O..O..O.OO...O#",
    "#..O.O.O...OO.#..OO..#O....O...#...OO...O.O.O#OO.#",
    "#O.OO#..O..........OO....#...#..#.#..O........O..#",
    "#..##..OOO...#.#.....##.O..O..#..#O.......O#.O.OO#",
    "#O.O.#.O.O..#.OO.#O..O...O..#..OO.OO..O.O..OOOOO.#",
    "#.#O..OOO#..#.....O.O...O...O.OO....OO.OO#O#O#O#O#",
    "#...O.O.#...#O....O.OOO...O.#.....O..O.O.......OO#",
    "#..OO.......O.O..OO#.....OOOOO.....O.OO..#O...OO##",
    "#.....O..#.O.......O...O....OO.O.........O.......#",
    "#..O.O.........O....O.#O.....#....#.O....#O......#",
    "#..##....O.O...O..O.O..##O...#....O#O.......OO#O##",
    "##OOOO.O.O.O.OO.........O..O........O#O..O....#OO#",
    "#..OO#...O...OO.....#.OOO....OO.OOO..O.O..OOO.O..#",
    "#O..OO..O..OO............O.#..#O..O...O..##OOOO#O#",
    "#...O..O#..O.O....O..O...O....#.#O.OO.O...#O.OOO.#",
    "#......O.....O...OO..##.O.O...O....O...O....O....#",
    "#O.O.#O.#O..O..OO.#O#..O..O..O.#.O...O.O...OO.O..#",
    "#.OO....#OO.O...O.O.#...#.O.O..O#......O........O#",
    "#....O.#O.OOO....O.O#..O.O.#..O.....O.OOO.O.#.##.#",
    "#..#O..#.....OO....O.O.O.O.O..#..O.O#..OO.....O..#",
    "#...O.............OO..#....#O......O..O.OO.O#.O.O#",
    "#...#..O..O...O.#O.O.O.....OOO..OO#......O..OOO..#",
    "##################################################"
    ]

p2Start = [
    "##############",
    "##......##..##",
    "##..........##",
    "##....[][]@.##",
    "##....[]....##",
    "##..........##",
    "##############"
    ]

p2Expected = [
    "##############",
    "##...[].##..##",
    "##...@.[]...##",
    "##....[]....##",
    "##..........##",
    "##..........##",
    "##############"
    ]

p2Input = "<vv<<^^<<^^"


movementTests' :: [TestCase Grid]
movementTests' = [
    TestCase {
        testName = "3",
        expected = p2Expected,
        actual = testHelper p2Start p2Input
    }
    ]

boxScore (x, y) = fromIntegral (y * 100 + x)

sumBoxes' grd = 
    let boxLocations = getCharacterLocations grd 'O'
        ys = sum $ map snd boxLocations
        xs = sum $ map fst boxLocations
        final = (ys * 100) + xs
    in trace ("Y total: " ++ show ys ++ 
             "\nX total: " ++ show xs ++ 
             "\nFinal: " ++ show final) $ 
       sum $ map boxScore boxLocations


-- let newPos = positionInDirection startPos dir 1
--                 targetChar = getAtLocation grid newPos
--             in case targetChar of
--                 '.' -> replaceAtLocation
--                     (replaceAtLocation grid newPos '@')
--                     startPos '.'
--                 b | b `elem` ['O', '[', ']'] -> case canPushBoxes grid dir newPos of
--                     Nothing -> grid  -- Can't push the boxes
--                     Just endPos -> pushBoxes grid dir startPos newPos endPos
--                 _ -> grid
testHelper' grid start dir = do
    let newPos = positionInDirection start dir 1
    canPushBoxes grid dir newPos

movementTests'' :: [TestCase (Maybe (Int, Int))]
movementTests'' = [
    TestCase {
        testName = "Single empty space",
        expected = Just (1,1),
        actual = testHelper' [
            "#########",
            "##.@[].##",
            "#########"
        ] (3, 1) RIGHT
    }
    ]