{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Days.Day16 (main) where
import Util (readInputFile)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (sortOn)
import Grid (Grid, Position, getCharacterLocations, getCharacterLocation, Direction(..))

data Action = Move | TurnLeft | TurnRight deriving (Eq, Ord, Show)
type State' = (Position, Direction)
type PathState = (Cost, State', [(State', Action)])
type Cost = Integer

knownBestScore :: Cost
knownBestScore = 92432

infiniteCost :: Cost
infiniteCost = 1000000000

actionCost :: Action -> Cost
actionCost Move = 1
actionCost _ = 1000

main :: IO ()
main = do
    input <- readInputFile "day16.txt"
    let grid = lines input
    -- let p1 = findMinCostPath grid
    -- case p1 of
    --     Just (cost, path) -> do
    --         print "Part 1:"
    --         print cost
    --     Nothing -> print "Failed"
    let p2 = findAllMinCostPaths grid
    case p2 of
        Just (cost, paths) -> do
            putStrLn $ "Minimum cost: " ++ show cost
            putStrLn $ "Number of minimum cost paths: " ++ show (length paths)

            let uniquePositions = getAllUniquePositions grid paths
            putStrLn $ "Number of unique positions visited: " ++ show (S.size uniquePositions)
        Nothing -> putStrLn "Failed"

moveForward :: Position -> Direction -> Position
moveForward (x, y) dir = case dir of
    UP    -> (x  , y-1)
    DOWN  -> (x  , y+1)
    RIGHT -> (x+1,   y)
    LEFT  -> (x-1,   y)

turn :: Direction -> Action -> Direction
turn dir TurnLeft = case dir of
    UP    -> LEFT
    LEFT  -> DOWN
    DOWN  -> RIGHT
    RIGHT -> UP
turn dir TurnRight = case dir of
    UP    -> RIGHT
    RIGHT -> DOWN
    DOWN  -> LEFT
    LEFT  -> UP
turn dir Move = dir

isValidPosition :: Grid -> Position -> Bool
isValidPosition grid pos@(x, y)
    | x < 0 || y < 0 || y >= length grid || x >= length (head grid) = False
    | otherwise = pos `notElem` getCharacterLocations grid '#'

getNextStates :: Grid -> State' -> [(State', Action)]
getNextStates grid (pos, dir) =
    moveStates ++ turnStates
  where
    nextPos = moveForward pos dir
    moveStates = [((nextPos, dir), Move) | isValidPosition grid nextPos]
    turnStates = [((pos, turn dir action), action) |
                 action <- [TurnLeft, TurnRight]]

getPathPositions :: [(State', Action)] -> S.Set Position
getPathPositions path = S.fromList $ map (fst . fst) path

getAllUniquePositions :: Grid -> [[(State', Action)]] -> S.Set Position
getAllUniquePositions grid paths =
    let pathPositions = S.unions $ map getPathPositions paths
        startPos = maybe S.empty S.singleton $ getCharacterLocation grid 'S'
        endPos = maybe S.empty S.singleton $ getCharacterLocation grid 'E'
    in S.unions [pathPositions, startPos, endPos]

findAllMinCostPaths :: Grid -> Maybe (Cost, [[(State', Action)]])
findAllMinCostPaths grid = do
    startPos <- getCharacterLocation grid 'S'
    endPos <- getCharacterLocation grid 'E'
    let startState = (startPos, RIGHT)
        result = dijkstraAllPaths grid endPos
                [(0, startState, [])]
                (M.singleton startState (0, [[]]))
                M.empty
    return result

dijkstraAllPaths :: Grid -> Position -> [PathState] -> M.Map State' (Cost, [[(State', Action)]]) -> M.Map State' Cost -> (Cost, [[(State', Action)]])
dijkstraAllPaths _ end [] costs _ =
    let endStates = [(cost, paths) |
                     ((pos, _), (cost, paths)) <- M.toList costs,
                     pos == end]
    in if null endStates
       then (infiniteCost, [])
       else let minCost = minimum $ map fst endStates
            in (minCost, concatMap snd $ filter ((== minCost) . fst) endStates)
dijkstraAllPaths grid end ((cost, state@(pos, _), path):queue) costs visited
    | cost > knownBestScore =
        dijkstraAllPaths grid end queue costs visited
    | pos == end =
        let (curCost, curPaths) = M.findWithDefault (infiniteCost, []) state costs
            newPaths
              | cost == curCost = path : curPaths
              | cost < curCost = [path]
              | otherwise = curPaths
            newCosts = M.insert state (min cost curCost, newPaths) costs
        in dijkstraAllPaths grid end queue newCosts visited
    | Just prevCost <- M.lookup state visited,
      prevCost < cost =
        dijkstraAllPaths grid end queue costs visited
    | otherwise =
        let nextMoves = getNextStates grid state
            validMoves = filter (\(newState, _) ->
                               case M.lookup newState visited of
                                 Just prevCost -> cost + actionCost Move <= prevCost
                                 Nothing -> True) nextMoves
            newStates = [(newCost, newState, path ++ [(state, action)]) |
                        (newState, action) <- validMoves,
                        let newCost = cost + actionCost action,
                        newCost <= knownBestScore]
            (curCost, curPaths) = M.findWithDefault (infiniteCost, []) state costs
            newPaths
              | cost == curCost = path : curPaths
              | cost < curCost = [path]
              | otherwise = curPaths
            newCosts = M.insert state (min cost curCost, newPaths) costs
            newVisited = M.insert state cost visited
            newQueue = mergeQueues queue newStates
        in dijkstraAllPaths grid end newQueue newCosts newVisited

mergeQueues :: [PathState] -> [PathState] -> [PathState]
mergeQueues [] new = sortOn (\(c,_,_) -> c) new
mergeQueues old [] = old
mergeQueues (x@(c1,_,_):xs) (y@(c2,_,_):ys)
    | c1 <= c2 = x : mergeQueues xs (y:ys)
    | otherwise = y : mergeQueues (x:xs) ys