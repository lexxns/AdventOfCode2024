import Grid (Direction, getDirectionalPositions, getCharacterLocations, getAtLocation)
import Data.List (find)

main :: IO ()
main = do
    map <- readFile "day6.txt"
    let grid = lines map
    let guardLocation = head $ getCharacterLocations grid '^'
    let obstacleLocations = getCharacterLocations grid '#'
    print "hi"

obstacleInPath :: [String] -> (Int, Int) -> [(Int, Int)] -> Direction -> Maybe (Int, Int)
obstacleInPath grid loc obstacles dir = 
    case getDirectionalPositions grid loc dir of
        [] -> Nothing
        (positions:_) -> findFirstObstacle positions
    where
        findFirstObstacle = find (\pos -> getAtLocation grid pos == '#')