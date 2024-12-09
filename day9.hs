import TestUtils
import Data.Ord (comparing)
import Data.Char (digitToInt)
import Data.List (sortBy, minimumBy, maximumBy)

data Block = Block {
    position :: Int,
    value :: Int
} deriving (Show, Eq)

type DiskMap = [Block]

main :: IO ()
main = do
    input <- readFile "day9.txt"
    let answer = calculateChecksum $ moveNumbers $ parseDiskMap input
    print answer

calculateChecksum :: DiskMap -> Integer
calculateChecksum diskMap =
    sum [fromIntegral (position block) * fromIntegral (value block) | block <- diskMap]

parseDiskMap :: String -> DiskMap
parseDiskMap = parse 0 0
  where
    parse _ _ [] = []
    parse blockId pos (f:s:rest) =
        let fileLen = digitToInt f
            spaceLen = digitToInt s
            -- Create blocks with consecutive positions
            fileBlocks = [Block {
                position = pos + i,
                value = blockId
            } | i <- [0..fileLen-1]]
            nextPos = pos + fileLen + spaceLen  -- Adjust next position
        in fileBlocks ++ parse (blockId + 1) nextPos rest
    parse blockId pos [f] =
        let fileLen = digitToInt f
        in [Block {
            position = pos + i,
            value = blockId
        } | i <- [0..fileLen-1]]

-- Find spaces in the disk map
findSpaces :: DiskMap -> [Int]
findSpaces blocks =
    let sortedBlocks = sortBy (comparing position) blocks
        maxPos = if null blocks
                then 0
                else maximum (map position blocks)
        occupied = map position sortedBlocks
    in [pos | pos <- [0..maxPos], pos `notElem` occupied]

-- Move a block to a new position
moveBlock :: Block -> Int -> DiskMap -> DiskMap
moveBlock block newPos diskMap =
    let oldPos = position block
        -- Shift blocks between old and new positions
        adjustBlock b
            | b == block = block { position = newPos }
            | position b > oldPos = b { position = position b - 1 }
            | otherwise = b
    in sortBy (comparing position) $ map adjustBlock diskMap

-- Find the next block to move
findNextMove :: DiskMap -> Maybe (Block, Int)
findNextMove diskMap =
    let spaces = findSpaces diskMap
        movableBlocks = filter (canMoveLeft diskMap) diskMap
    in case (spaces, movableBlocks) of
        ([], _) -> Nothing
        (_, []) -> Nothing
        (_, blocks) ->
            let rightmostBlock = maximumBy (comparing position) blocks
                validSpaces = [pos | pos <- spaces, pos < position rightmostBlock]
            in case validSpaces of
                [] -> Nothing
                xs -> Just (rightmostBlock, minimum xs)

-- Check if a block can move left
canMoveLeft :: DiskMap -> Block -> Bool
canMoveLeft diskMap block =
    let spaces = findSpaces diskMap
    in any (< position block) spaces

-- Make all possible moves
moveNumbers :: DiskMap -> DiskMap
moveNumbers = mv
  where
    mv currentMap = case findNextMove currentMap of
        Nothing -> currentMap
        Just (block, newPos) -> mv (moveBlock block newPos currentMap)

-- Convert DiskMap to string representation
diskMapToString :: DiskMap -> String
diskMapToString blocks =
    let maxPos = if null blocks
                then 0
                else maximum (map position blocks)
        emptyString = replicate (maxPos + 1) '.'
    in foldr (\block str ->
        let (before, _:after) = splitAt (position block) str
        in before ++ show (value block) ++ after) emptyString blocks

-- Test cases for parsing
diskMapParsing :: [TestCase String]
diskMapParsing = [
    TestCase {
        testName = "12345",
        expected = "0..111....22222",
        actual = diskMapToString $ parseDiskMap "12345"
    },
    TestCase {
        testName = "2333133121414131402",
        expected = "00...111...2...333.44.5555.6666.777.888899",
        actual = diskMapToString $ parseDiskMap "2333133121414131402"
    }
    ]
-- Test cases for reordering
reordering :: [TestCase String]
reordering = [
    TestCase {
        testName = "Simple reordering: 12345",
        expected = "022111222",
        actual = diskMapToString $ moveNumbers $ parseDiskMap "12345"
    },
    TestCase {
        testName = "Complex reordering: 2333133121414131402",
        expected = "0099811188827773336446555566",
        actual = diskMapToString $ moveNumbers $ parseDiskMap "2333133121414131402"
    },
    TestCase {
        testName = "Double Digits: 233313312141413140255",
        expected = "00101010111101092988333844855557666677",
        actual = diskMapToString $ moveNumbers $ parseDiskMap "233313312141413140255"
    }
    ]

-- Test cases for checksum
checksum :: [TestCase Integer]
checksum = [
    TestCase {
        testName = "Simple checksum: 12345",
        expected = 60,
        actual = calculateChecksum $ moveNumbers $ parseDiskMap "12345"
    },
    TestCase {
        testName = "Complex checksum: 2333133121414131402",
        expected = 1928,
        actual = calculateChecksum $ moveNumbers $ parseDiskMap "2333133121414131402"
    }
    ]
