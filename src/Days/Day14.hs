{-# LANGUAGE OverloadedStrings #-}
module Days.Day14 (main) where
import TestUtils
import Util (readInputFile)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T
import Grid (Grid, Vector, Position, addVectorToPointWrapped, visualizeGrid, gridDimensions, setAtLocation)

type Parser = Parsec Void Text


pNumber :: Parser Int
pNumber = L.signed space L.decimal

pCoordPair :: Parser (Int, Int)
pCoordPair = do
    x <- pNumber
    char ','
    y <- pNumber
    return (x, y)

pLine :: Parser (Position, Vector)
pLine = do
    string "p="
    pos <- pCoordPair
    space
    string "v="
    vel <- pCoordPair
    return (pos, vel)

pInput :: Parser [(Position, Vector)]
pInput = pLine `sepEndBy` newline

parseInput :: Text -> Either (ParseErrorBundle Text Void) [(Position, Vector)]
parseInput = parse pInput ""

calculatePath :: Grid -> (Position, Vector) -> [Position]
calculatePath grid (startPos, vec) = iterate (flip (addVectorToPointWrapped grid) vec) startPos

data Robot = Robot 
    { position :: Position
    , velocity :: Vector
    , symbol :: Char
    } deriving (Show)

parseRobots :: Text -> Either (ParseErrorBundle Text Void) [Robot]
parseRobots input = do
    positions <- parseInput input
    return $ zipWith (\((x,y), v) i -> Robot (x,y) v (head (show i))) positions [1..]

data Quadrant = Q1 | Q2 | Q3 | Q4
  deriving (Show, Eq)

findMiddle :: Int -> Int
findMiddle n = n `div` 2

-- | Function to determine which quadrant a position belongs to
-- Returns Nothing if the position is on the middle lines
getQuadrant :: Int -> Int -> Position -> Maybe Quadrant
getQuadrant gridWidth gridHeight (x, y) = 
    let midX = findMiddle gridWidth
        midY = findMiddle gridHeight
    in if x == midX || y == midY
       then Nothing  -- Position is on middle lines
       else Just $ case (x < midX, y < midY) of
                    (True,  True)  -> Q2  -- Top left
                    (False, True)  -> Q1  -- Top right
                    (True,  False) -> Q3  -- Bottom left
                    (False, False) -> Q4  -- Bottom right

-- | Count positions in each quadrant
countQuadrants :: Int -> Int -> [Position] -> (Int, Int, Int, Int)
countQuadrants width height positions = 
    let counts = foldr countPosition (0, 0, 0, 0) positions
    in counts
    where
        countPosition pos (q1, q2, q3, q4) =
            case getQuadrant width height pos of
                Just Q1 -> (q1 + 1, q2, q3, q4)
                Just Q2 -> (q1, q2 + 1, q3, q4)
                Just Q3 -> (q1, q2, q3 + 1, q4)
                Just Q4 -> (q1, q2, q3, q4 + 1)
                Nothing -> (q1, q2, q3, q4)  -- Ignore middle lines

countRobotAreas :: Grid -> [Position] -> Int
countRobotAreas grid poss = do
    let (width, height) = gridDimensions grid
    let (q1, q2, q3, q4) = countQuadrants width height poss
    q1 * q2 * q3 * q4


main :: IO ()
main = do
    let emptyGrid = replicate 103 (replicate 101 '.')
    content <- readInputFile "day14.txt"
    case parseRobots (T.pack content) of
        Left err -> putStr $ errorBundlePretty err
        Right robots -> do
            -- Calculate all paths
            let paths = map (\robot -> calculatePath emptyGrid (position robot, velocity robot)) robots
            let initialPositions = map position robots
            let finalPositions = map (last . take 101) paths
            let symbols = map symbol robots
            
            -- Create initial grid
            let initialGrid = foldr (\(pos, sym) grid -> setAtLocation grid pos sym) 
                                  emptyGrid 
                                  (zip initialPositions symbols)
            
            -- Create final grid
            let finalGrid = foldr (\(pos, sym) grid -> setAtLocation grid pos sym) 
                                emptyGrid 
                                (zip finalPositions symbols)

            let safety = countRobotAreas emptyGrid finalPositions
            
            putStrLn "Initial positions:"
            visualizeGrid initialGrid
            putStrLn "\nFinal positions after 100 steps:"
            visualizeGrid finalGrid

            print safety

exampleGrid :: Grid
exampleGrid = [
    "1.12.......",
    "...........",
    "...........",
    "......11.11",
    "1.1........",
    ".........1.",
    ".......1..."
    ]