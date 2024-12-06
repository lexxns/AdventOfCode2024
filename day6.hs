import Grid (Direction, getDirectionalPositions, getCharacterLocations, getAtLocation, orthogonalDirections)
import Data.List (find)
import Debug.Trace (trace)
import qualified Data.Set as Set
import Util (dropLast)

main :: IO ()
main = do
    map <- readFile "day6.txt"
    let grid = lines map
    let guardLocation = head $ getCharacterLocations grid '^'
    let obstacleLocations = getCharacterLocations grid '#'
    let path = followGuardPath grid obstacleLocations guardLocation
    let uniqueLocations = Set.fromList path :: Set.Set (Int, Int)
    print "Part 1:"
    print $ length uniqueLocations

obsLocation :: [String] -> [(Int, Int)] -> Maybe (Int, Int)
obsLocation grid = find (\pos -> getAtLocation grid pos == '#')

followGuardPath :: [String] -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
followGuardPath grid obstacles startPos = move startPos (cycle orthogonalDirections) []
  where
    move pos (dir:restDirs) acc = 
      trace ("Current pos: " ++ show pos ++ "\nDirection: " ++ show dir) $
      let positions = trace ("Generated positions: " ++ show positions) $ 
                     getDirectionalPositions grid pos dir
      in case obsLocation grid positions of
           Nothing -> trace "No obstacle found, returning all positions" $ acc ++ positions
           Just obstacle -> 
             let path = dropLast $ takeWhile (/= obstacle) positions
                 newPos = if null path then pos else last path
             in trace ("Obstacle found at: " ++ show obstacle ++ "\nPath to obstacle: " ++ show path ++ 
                      "\nNew position: " ++ show newPos) $
                move newPos restDirs (acc ++ path)