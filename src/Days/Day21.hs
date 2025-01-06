{-# LANGUAGE InstanceSigs #-}
module Days.Day21 (main) where
import Util (readInputFile, allListPairs, allEnumValues)
import Graph (addConnections, Graph, empty, shortestRoutes, combineRoutes)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (minimumBy, nub)
import qualified Data.Map as Map
import Data.Ord (comparing)

data Direction = UP | RIGHT |  DOWN |  LEFT | A
    deriving (Eq, Ord, Enum, Bounded)

instance Show Direction where
    show :: Direction -> String
    show UP = "^"
    show LEFT = "<"
    show RIGHT = ">"
    show DOWN = "v"
    show A = "A"

directionsToString :: [Direction] -> String
directionsToString = concatMap show

primaryKeypad :: Graph Char
primaryKeypad = addConnections [
    ('A', '0'), ('A', '3'), ('0', '2'), ('3', '2'), ('2', '1'), ('3', '6'),
    ('2', '5'), ('6', '5'), ('6', '9'), ('5', '4'), ('5', '8'), ('9', '8'),
    ('4', '7'), ('8', '7')
    ] empty 

secondaryKeypad :: Graph Direction
secondaryKeypad = addConnections [
    (LEFT, DOWN), (DOWN, RIGHT), (UP, DOWN), (UP, A), (A, RIGHT)
    ] empty

primaryNodes :: [Char]
primaryNodes = nub $ concatMap (\(a, b) -> [a, b]) primaryConnections
  where
    primaryConnections = [
        ('A', '0'), ('A', '3'), ('0', '2'), ('3', '2'), ('2', '1'), ('3', '6'),
        ('2', '5'), ('6', '5'), ('6', '9'), ('5', '4'), ('5', '8'), ('9', '8'),
        ('4', '7'), ('8', '7')
        ]

primaryPathsMap :: Map.Map (Char, Char) [[Char]]
primaryPathsMap = Map.fromList [
    (pair, uncurry shortestRoutes pair primaryKeypad)
    | pair <- allListPairs primaryNodes
    ]

secondaryPathsMap :: Map.Map (Direction, Direction) [[Direction]]
secondaryPathsMap = Map.fromList [
    (pair, uncurry shortestRoutes pair secondaryKeypad)
    | pair <- allListPairs allEnumValues
    ]

primaryLookupMap :: Map.Map (Char, Char) Direction
primaryLookupMap = Map.fromList [
    (('A', '3'), UP),
    (('3', 'A'), DOWN),
    (('A', '0'), LEFT),
    (('0', 'A'), RIGHT),
    (('0', '2'), UP),
    (('2', '0'), DOWN),
    (('1', '2'), RIGHT),
    (('2', '1'), LEFT),
    (('2', '3'), RIGHT),
    (('3', '2'), LEFT),
    (('4', '5'), RIGHT),
    (('5', '4'), LEFT),
    (('5', '6'), RIGHT),
    (('6', '5'), LEFT),
    (('7', '8'), RIGHT),
    (('8', '7'), LEFT),
    (('8', '9'), RIGHT),
    (('9', '8'), LEFT),
    (('1', '4'), UP),
    (('4', '1'), DOWN),
    (('2', '5'), UP),
    (('5', '2'), DOWN),
    (('3', '6'), UP),
    (('6', '3'), DOWN),
    (('4', '7'), UP),
    (('7', '4'), DOWN),
    (('5', '8'), UP),
    (('8', '5'), DOWN),
    (('6', '9'), UP),
    (('9', '6'), DOWN)
    ]

secondaryLookupMap :: Map.Map (Direction, Direction) Direction
secondaryLookupMap = Map.fromList [
    ((LEFT, DOWN), RIGHT),
    ((DOWN, LEFT), LEFT),
    ((DOWN, RIGHT), RIGHT),
    ((RIGHT, DOWN), LEFT),
    ((UP, DOWN), DOWN),
    ((DOWN, UP), UP),
    ((UP, A), RIGHT),
    ((A, UP), LEFT),
    ((A, RIGHT), DOWN),
    ((RIGHT, A), UP)
    ]

getDirection :: Char -> Char -> Maybe Direction
getDirection from to = Map.lookup (from, to) primaryLookupMap

pathToDirections :: [Char] -> Maybe [Direction]
pathToDirections [] = Just []
pathToDirections [_] = Just [A]
pathToDirections (x:y:rest) = do
    dir <- getDirection x y
    remainingDirs <- pathToDirections (y:rest)
    return (dir : remainingDirs)

processSubroutes :: [[Char]] -> [[Direction]]
processSubroutes = mapMaybe pathToDirections

combineDirectionPaths :: [[[Direction]]] -> [[Direction]]
combineDirectionPaths [] = []
combineDirectionPaths [x] = x
combineDirectionPaths (x:xs) = do
    p1 <- x
    p2 <- combineDirectionPaths xs
    return $ p1 ++ p2

getSecondaryDirection :: Direction -> Direction -> Maybe Direction
getSecondaryDirection from to = Map.lookup (from, to) secondaryLookupMap

pathToSecondaryDirections :: [Direction] -> Maybe [Direction]
pathToSecondaryDirections [] = Just []
pathToSecondaryDirections [_] = Just [A]  -- Same terminal case as primary
pathToSecondaryDirections (x:y:rest) = do
    dir <- getSecondaryDirection x y
    remainingDirs <- pathToSecondaryDirections (y:rest)
    return (dir : remainingDirs)

processSecondarySubroutes :: [[Direction]] -> [[Direction]]
processSecondarySubroutes = mapMaybe pathToSecondaryDirections

getPreCalculatedPath :: (Char, Char) -> Maybe [[Char]]
getPreCalculatedPath key = Map.lookup key primaryPathsMap

getPreCalculatedSecondaryPath :: (Direction, Direction) -> Maybe [[Direction]]
getPreCalculatedSecondaryPath key = Map.lookup key secondaryPathsMap

findSecondaryRoutes :: [[Direction]] -> [[Direction]]
findSecondaryRoutes primaryRoutes = 
    [ combinedRoute 
    | primaryPath <- primaryRoutes
    , let routePairs = zip (A : primaryPath) primaryPath
    , let routes = mapMaybe getPreCalculatedSecondaryPath routePairs
    , combinedRoute <- combineDirectionPaths (map processSecondarySubroutes routes)
    ]

main :: IO ()
main = do
    -- Get pre-calculated paths
    let p1 = fromMaybe [] $ getPreCalculatedPath ('A', '0')
    let p2 = fromMaybe [] $ getPreCalculatedPath ('0', '2')
    let p3 = fromMaybe [] $ getPreCalculatedPath ('2', '9')
    let p4 = fromMaybe [] $ getPreCalculatedPath ('9', 'A')

    let primaryRoutes = combineDirectionPaths (map processSubroutes [p1, p2, p3, p4])
    let secondaryRoutes = findSecondaryRoutes primaryRoutes
    -- let tertiaryRoutes = findSecondaryRoutes secondaryRoutes
    -- let quaternaryRoutes = findSecondaryRoutes tertiaryRoutes
    
    -- Find and print shortest route
    let shortestRoute = minimumBy (comparing length) secondaryRoutes
    print $ directionsToString shortestRoute