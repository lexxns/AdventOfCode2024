module Days.Day07 (main) where

import Data.List
import TestUtils
import Util (readInputFile)

data CalibrationData = CalibrationData
    { calibration :: Integer
    , inputs :: [Integer]
    } deriving (Show)

parseLine :: String -> CalibrationData
parseLine line = CalibrationData cal nums
  where
    (calStr, rest) = break (==':') line
    numsStr = dropWhile (==':') rest
    cal = read (trim calStr)
    nums = map read . words $ trim numsStr

parseFile :: String -> [CalibrationData]
parseFile = map parseLine . lines

trim :: String -> String
trim = dropWhile (==' ') . reverse . dropWhile (==' ') . reverse

main :: IO ()
main = do
    content <- readInputFile "day7.txt"
    let cd = parseFile content
    let total = sumValidCalibrations cd
    print total

type Operator = Integer -> Integer -> Integer

concatTwoIntToStr :: Integer -> Integer -> Integer
concatTwoIntToStr n1 n2 = read $ show n1 ++ show n2

operatorCombinations :: Int -> [[Integer -> Integer -> Integer]]
operatorCombinations n = sequence $ replicate n [(+), (*), (concatTwoIntToStr)]

applyOperations :: [Operator] -> [Integer] -> Integer
applyOperations ops nums
    | length ops /= length nums - 1 = error "Need one less operator than numbers"
    | otherwise = foldl applyOp (head nums) (zip ops (tail nums))
    where
        applyOp acc (op, n) = op acc n

hasSolution :: CalibrationData -> Bool
hasSolution cd
    = calculate (operatorCombinations (length (inputs cd) - 1))
    where
        calculate [] = False
        calculate (ops:rest)
            | applyOperations ops (inputs cd) == calibration cd = True
            | otherwise = calculate rest

sumValidCalibrations :: [CalibrationData] -> Integer
sumValidCalibrations cds = sum [calibration cd | cd <- cds, hasSolution cd]

hasSolutionTests :: [TestCase Bool]
hasSolutionTests = [
    TestCase {
        testName = "1 + 1 + 1",
        expected = True,
        actual = hasSolution (CalibrationData 3 [1, 1, 1])
    },
    TestCase {
        testName = "2 * 3 = 6",
        expected = True,
        actual = hasSolution (CalibrationData 6 [2, 3])
    },
    TestCase {
        testName = "156: 15 6",
        expected = True,
        actual = hasSolution (CalibrationData 156 [15, 6])
    },
    TestCase {
        testName = "7290: 6 8 6 15",
        expected = True,
        actual = hasSolution (CalibrationData 7290 [6, 8, 6, 15])
    },
    TestCase {
        testName = "2 + 3 = 5",
        expected = True,
        actual = hasSolution (CalibrationData 5 [2, 3])
    },
    TestCase {
        testName = "161011: 16 10 13",
        expected = False,
        actual = hasSolution (CalibrationData 161011 [16, 10, 13])
    },
    TestCase {
        testName = "2 * 3 + 2 = 8",
        expected = True,
        actual = hasSolution (CalibrationData 8 [2, 3, 2])
    },
    TestCase {
        testName = "2 * 3 * 2 = 12",
        expected = True,
        actual = hasSolution (CalibrationData 12 [2, 3, 2])
    },
    TestCase {
        testName = "2 + 3 + 2 = 7",
        expected = True,
        actual = hasSolution (CalibrationData 7 [2, 3, 2])
    },
    TestCase {
        testName = "Target less than product impossible",
        expected = False,
        actual = hasSolution (CalibrationData 3 [2, 2])
    },
    TestCase {
        testName = "Target equals product possible",
        expected = True,
        actual = hasSolution (CalibrationData 4 [2, 2])
    },
    TestCase {
        testName = "3267: 81 40 27",
        expected = True,
        actual = hasSolution (CalibrationData 3267 [81, 40, 27])
    },
    TestCase {
        testName = "292: 11 6 16 20",
        expected = True,
        actual = hasSolution (CalibrationData 292 [11, 6, 16, 20])
    }
    ]