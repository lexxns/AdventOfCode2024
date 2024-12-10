import Grid
import Data.Char (digitToInt)
import Data.Maybe (catMaybes)
import Debug.Trace (trace)
import qualified Data.Set as Set

data PathState = PathState {
    startPos :: Position,
    currentPath :: [Position],
    completedPaths :: Set.Set [Position]
} deriving (Show)

canBeCompleted :: Grid -> PathState -> (Position, Int) -> PathState
canBeCompleted grid state (pos, target)
  | target == 0 = state { 
      completedPaths = Set.insert (reverse (pos : currentPath state)) (completedPaths state)
    }
  | target < 0 = state
  | pos `elem` currentPath state = state
  | otherwise = trace ("Checking " ++ show pos ++ " target=" ++ show target) $
      foldr combinePaths state { currentPath = pos : currentPath state } steps
  where
    adjacent = concat $ getOrthogonalPositions grid pos 1
    steps = [canBeCompleted grid state (adjPos, target - 1) |
             adjPos <- adjacent,
             let val = digitToInt (getAtLocation grid adjPos),
             val == target - 1]
    
    combinePaths s1 s2 = PathState {
        startPos = startPos state,
        currentPath = currentPath state,
        completedPaths = completedPaths s1 `Set.union` completedPaths s2
    }

getTrailEnds :: Grid -> [Position]
getTrailEnds grid = getCharacterLocations grid '9'

main :: IO ()
main = do
    mapContent <- readFile "day10.txt"
    let grid = lines mapContent
        trailEnds = getTrailEnds grid
        pathStates = [canBeCompleted grid (PathState {  startPos = pos,  currentPath = [], completedPaths = Set.empty  })  (pos, 9) | pos <- trailEnds]
        totalPaths = sum $ map (Set.size . completedPaths) pathStates
    
    mapM_ (\state -> do
        putStrLn $ "\nPaths from " ++ show (startPos state) ++ ":"
        mapM_ print (Set.toList $ completedPaths state)
        ) pathStates

    print "Day 10:" 
    print $ show totalPaths