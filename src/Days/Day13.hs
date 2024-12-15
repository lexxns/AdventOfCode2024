{-# LANGUAGE OverloadedStrings #-}
module Days.Day13 (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (void)
import Util (readInputFile, getInputPath)
import qualified Data.Map.Strict as M
import qualified Control.Monad.Trans.State as S
import Control.Applicative ((<|>))
import Grid (getManhattanDistance)
import TestUtils

type Position = (Integer, Integer)

type ButtonCombo = Maybe Integer
type Dyn = S.State (M.Map (Position, Integer) ButtonCombo)
type Parser = Parsec Void T.Text

minOf :: ButtonCombo -> ButtonCombo -> ButtonCombo
minOf (Just i) (Just j) = Just (min i j)
minOf c1 c2             = c1 <|> c2

acost::Integer
acost = 3

bcost::Integer
bcost = 1

conversion::Integer
conversion = 10000000000000

data Entry = Entry
    { buttonA :: Position
    , buttonB :: Position
    , prize :: Position
    } deriving (Show, Eq)

buttonPosition :: Parser Position
buttonPosition = do
    void $ char 'X'
    void $ char '+'
    x <- L.decimal
    void $ string ", "
    void $ char 'Y'
    void $ char '+'
    y <- L.decimal
    return (x, y)

prizePosition :: Parser Position
prizePosition = do
    void $ char 'X'
    void $ char '='
    x <- L.decimal
    void $ string ", "
    void $ char 'Y'
    void $ char '='
    y <- L.decimal
    return (x + conversion, y + conversion)

buttonLine :: T.Text -> Parser Position
buttonLine name = do
    void $ string name
    void $ string ": "
    buttonPosition

prizeLine :: Parser Position
prizeLine = do
    void $ string "Prize: "
    prizePosition

parseEntry :: Parser Entry
parseEntry = do
    bA <- buttonLine "Button A"
    void newline
    bB <- buttonLine "Button B"
    void newline
    Entry bA bB <$> prizeLine

entries :: Parser [Entry]
entries = parseEntry `sepBy` count 2 newline

parseFile :: FilePath -> IO (Either (ParseErrorBundle T.Text Void) [Entry])
parseFile filepath = do
    content <- TIO.readFile filepath
    return $ parse entries filepath content

findCostDP :: Position -> Position -> Integer -> Entry -> Dyn ButtonCombo
findCostDP curr@(x, y) target@(tx, ty) stepsLeft entry
    | stepsLeft < 0 = return Nothing
    | curr == target = return (Just 0)
    | otherwise = do
        pressA <- tryPress (buttonA entry) acost
        pressB <- tryPress (buttonB entry) bcost
        return $ minOf pressA pressB
    where
        tryPress :: Position -> Integer -> Dyn ButtonCombo
        tryPress (dx, dy) cost = do
            let next@(nx, ny) = (x + dx, y + dy)
            (if (((
                (manhattan next target > stepsLeft) 
                || (tx > x && nx > tx)) 
                || (tx < x && nx < tx)) 
                || (ty > y && ny > ty)) 
                || (ty < y && ny < ty) 
            then return Nothing else (do
                subCost <- memorize next target (stepsLeft - 1) entry
                return $ fmap (+cost) subCost))

        manhattan (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

memorize :: Position -> Position -> Integer -> Entry -> Dyn ButtonCombo
memorize pos target steps entry = do
    val <- do
        elem <- S.gets $ M.lookup (pos, steps)
        case elem of
            Just x  -> return x
            Nothing -> findCostDP pos target steps entry
    S.modify $ M.insert (pos, steps) val
    return val

findMinCost :: Entry -> Integer
findMinCost entry =
    Data.Maybe.fromMaybe 0
    (S.evalState
    (findCostDP (0, 0) (prize entry) maxSteps entry) M.empty)
    where
        maxSteps = manhattan (0, 0) (prize entry)
        manhattan (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)


findMinCostPart2 :: Entry -> Integer
findMinCostPart2 Entry{buttonA=(ax,ay), buttonB=(bx,by), prize=(tx,ty)} = 
    if isValidSolution then round (3 * countA + countB) else 0
    where
        dax = fromIntegral ax :: Double
        day = fromIntegral ay :: Double
        dbx = fromIntegral bx :: Double
        dby = fromIntegral by :: Double
        dtx = fromIntegral tx :: Double
        dty = fromIntegral ty :: Double
        
        countA = (dtx * dby - dty * dbx) / (dax * dby - day * dbx)
        countB = (dtx - dax * countA) / dbx
        
        isValidSolution = 
            countA >= 0 && countB >= 0 &&
            abs (countA - fromIntegral (round countA)) < 1e-10 && 
            abs (countB - fromIntegral (round countB)) < 1e-10

main :: IO ()
main = do
    path <- getInputPath "day13.txt"
    result <- parseFile path
    case result of
        Left err -> putStrLn $ "Parse error: " ++ errorBundlePretty err
        Right es -> do
            -- let p1 = map findMinCost es
            -- print "Part 1:"
            -- print $ sum p1
            let p2 = map findMinCostPart2 es
            print "Part 2:"
            print $ sum p2
            

example :: Entry
example = Entry (94, 34) (22, 67) (8400, 5400)

fexample :: Entry
fexample = Entry (24, 66) (67, 21) (12748, 12176)

example' :: Entry
example' = Entry (94, 34) (22, 67) (8400 + conversion, 5400 + conversion)

fexample' :: Entry
fexample' = Entry (24, 66) (67, 21) (12748 + conversion, 12176 + conversion)

costTests :: [TestCase Integer]
costTests = [
    TestCase {
        testName = "1",
        expected = 280,
        actual = findMinCost example
    },
    TestCase {
        testName = "2",
        expected = 0,
        actual = findMinCost fexample
    },
        TestCase {
        testName = "1'",
        expected = 280,
        actual = findMinCost example'
    },
    TestCase {
        testName = "2'",
        expected = 0,
        actual = findMinCost fexample'
    }
    ]