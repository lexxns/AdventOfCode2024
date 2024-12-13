import Grid
import Data.Char (digitToInt)
import Data.Maybe (catMaybes)
import Debug.Trace (trace)

canBeCompleted :: Grid -> (Position, Int) -> Int
canBeCompleted grid (pos, target)
  | target == 0 = trace ("Found path to 0 at " ++ show pos) 1
  | target < 0 = 0
  | otherwise = trace ("Checking " ++ show pos ++ " target=" ++ show target) $ sum steps
  where
    adjacent = concat $ getOrthogonalPositions grid pos
    steps = [canBeCompleted grid (adjPos, target - 1) |
             adjPos <- adjacent,
             let val = digitToInt (getAtLocation grid adjPos),
             val == target - 1]
             
getTrailEnds :: Grid -> [Position]
getTrailEnds grid = getCharacterLocations grid '9'

main :: IO ()
main = do
    mapContent <- readFile "day10.txt"
    let grid = lines mapContent
        trailEnds = getTrailEnds grid
        totalPaths = sum [canBeCompleted grid (pos, 9) | pos <- trailEnds]
    print "part 2" 
    print $ show totalPaths