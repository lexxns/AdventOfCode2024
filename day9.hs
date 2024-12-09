import Data.Char (isDigit)
import Data.List (sortBy)
import Data.Ord (comparing)

main :: IO ()
main = do
    diskmap <- readFile "day9.txt"
    let positions = translateDiskmap diskmap
    print "Task 1"
    print $ calculateChecksum positions

-- Each number position is (starting_index, position_number, count)
type NumberPos = (Int, Int, Int)
-- Each space section is (starting_index, length)
type SpacePos = (Int, Int)

translateDiskmap :: String -> ([NumberPos], [SpacePos])
translateDiskmap input =
    let pairs = zipWith (\idx digit -> (idx, read [digit] :: Int)) [0..] input

        -- Helper function to calculate starting indices
        calculatePositions :: [(Int, Int)] -> ([NumberPos], [SpacePos])
        calculatePositions [] = ([], [])
        calculatePositions pairs =
            let positions = scanl (\currentPos (idx, count) -> currentPos + count) 0 pairs
                numbers = [(pos, idx `div` 2, count)
                         | ((idx, count), pos) <- zip pairs positions
                         , even idx]
                spaces = [(pos, count)
                        | ((idx, count), pos) <- zip pairs positions
                        , odd idx]
            in (numbers, spaces)

    in calculatePositions pairs

calculateChecksum :: ([NumberPos], [SpacePos]) -> Integer
calculateChecksum (numbers, _) =
    sum [fromIntegral pos * fromIntegral idx | (idx, pos, _) <- numbers]

moveNumbers :: ([NumberPos], [SpacePos]) -> ([NumberPos], [SpacePos])
moveNumbers (numbers, spaces) =
    let
        -- Sort numbers by index in descending order to process from right to left
        sortedNumbers = sortBy (flip $ comparing (\(idx, _, _) -> idx)) numbers
        
        -- Helper function to check if a position range is available
        isRangeAvailable :: Int -> Int -> [(Int, Int)] -> Bool
        isRangeAvailable start count usedRanges =
            not $ any (\(usedStart, usedEnd) -> 
                      (start >= usedStart && start < usedEnd) ||
                      (start + count > usedStart && start + count <= usedEnd) ||
                      (start <= usedStart && start + count >= usedEnd)) usedRanges

        -- Helper function to move a single number
        moveNumber :: NumberPos -> [SpacePos] -> [(Int, Int)] -> (NumberPos, [(Int, Int)])
        moveNumber num@(_, val, count) availableSpaces usedRanges =
            let
                -- Try each position from left to right until we find a valid spot
                tryPositions = [(idx, idx + spaceLen) | (idx, spaceLen) <- sortBy (comparing fst) availableSpaces]
                validPos = find (\(start, _) -> 
                               isRangeAvailable start count usedRanges) tryPositions
            in case validPos of
                Just (start, _) -> ((start, val, count), (start, start + count) : usedRanges)
                Nothing -> (num, usedRanges)  -- Keep original position if no valid spot found

        -- Process all numbers in sequence
        moveAllNumbers :: [NumberPos] -> [SpacePos] -> [(Int, Int)] -> [NumberPos]
        moveAllNumbers [] _ _ = []
        moveAllNumbers (num:nums) avSpaces usedRanges =
            let (movedNum, newUsedRanges) = moveNumber num avSpaces usedRanges
            in movedNum : moveAllNumbers nums avSpaces newUsedRanges

        -- Find first matching element
        find :: (a -> Bool) -> [a] -> Maybe a
        find _ [] = Nothing
        find pred (x:xs) = if pred x then Just x else find pred xs

    in (moveAllNumbers sortedNumbers spaces [], spaces)

-- Helper function to convert positions back to string
positionsToString :: [NumberPos] -> Int -> String
positionsToString numbers maxLen = 
    let positions = [(idx, show val) | (idx, val, count) <- numbers, 
                                     i <- replicate count val,
                                     idx <- [idx..idx + count - 1]]
        sortedPos = sortBy (comparing fst) positions
    in concatMap snd sortedPos