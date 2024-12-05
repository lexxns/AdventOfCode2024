import TestUtils

middle :: [a] -> a
middle l@(_:_:_) = middle $ tail $ init l
middle [x] = x

splitAtIndex :: Int -> [a] -> ([a], a, [a])
splitAtIndex 0 (x:xs) = ([], x, xs)
splitAtIndex n (x:xs) =
    let (front, elem, back) = splitAtIndex (n-1) xs
    in (x:front, elem, back)
splitAtIndex _ [] = error "Index out of bounds"

checkPageAgainstRules :: [(Int, Int)] -> [Int] -> Int -> Bool
checkPageAgainstRules rules update index = do
    let (left, page, right) = splitAtIndex index update
    -- Find rules where current page must come before something
    let mustPreceed = filter (\(x, _) -> x == page) rules
    -- Check that none of these numbers are in left
    all (\(_, mustFollow) -> mustFollow `notElem` left) mustPreceed

isValidOrder :: [(Int, Int)] -> [Int] -> Bool
isValidOrder rules update =
    all (checkPageAgainstRules rules update) [1..(length update - 1)]

passingUpdates :: [(Int, Int)] -> [[Int]] -> [[Int]]
passingUpdates rules updates = filter (isValidOrder rules) updates

parseRule :: String -> (Int, Int)
parseRule line =
    let [x, y] = map read $ words $ map (\c -> if c == '|' then ' ' else c) line
    in (x, y)

parseUpdate :: String -> [Int]
parseUpdate line = map read $ words $ map (\c -> if c == ',' then ' ' else c) line

readRulesFromFile :: FilePath -> IO [(Int, Int)]
readRulesFromFile path = do
    contents <- readFile path
    return $ map parseRule $ lines contents

readUpdatesFromFile :: FilePath -> IO [[Int]]
readUpdatesFromFile path = do
    contents <- readFile path
    return $ map parseUpdate $ lines contents

main :: IO ()
main = do
    rules <- readRulesFromFile "day5rules.txt"
    updates <- readUpdatesFromFile "day5updates.txt"
    let validUpdates = passingUpdates rules updates
    let middleNumbers = map middle validUpdates
    print $ sum middleNumbers

orderTests :: [TestCase Bool]
orderTests = [
    TestCase "Single rule, valid order"
        True
        (isValidOrder [(1,2)] [1,2]),

    TestCase "Single rule, invalid order"
        False
        (isValidOrder [(1,2)] [2,1]),

    TestCase "Multiple rules, valid order"
        True
        (isValidOrder [(1,2), (2,3)] [1,2,3]),

    TestCase "Multiple rules, invalid order"
        False
        (isValidOrder [(1,2), (2,3)] [1,3,2]),

    TestCase "Multiple rules with gaps, valid order"
        True
        (isValidOrder [(1,3), (3,5)] [1,2,3,4,5]),

    TestCase "Transitive relationships, valid order"
        True
        (isValidOrder [(1,2), (2,3), (1,3)] [1,2,3]),

    TestCase "Non-consecutive numbers, valid order"
        True
        (isValidOrder [(1,5), (5,10)] [1,3,5,7,10]),

    TestCase "Non-consecutive numbers, invalid order"
        False
        (isValidOrder [(1,5), (5,10)] [1,10,5]),

    TestCase "Multiple dependencies on same number, valid"
        True
        (isValidOrder [(1,3), (2,3)] [1,2,3]),

    TestCase "Multiple dependencies on same number, invalid"
        False
        (isValidOrder [(1,3), (2,3)] [3,1,2])
    ]
