module Days.Day23 (main) where
import Util (readInputFile)
import Data.List (intercalate)
import Graph (Graph, connect, empty, findCycles, normalizePaths, nodeMatches, maximumClique)

buildGraphFromLines :: [String] -> Graph String
buildGraphFromLines = foldr addConnection empty
  where
    addConnection line graph = 
      case break (=='-') (filter (`notElem` "\r\n") line) of
        (node1, '-':node2) -> connect node1 node2 graph
        _ -> graph


main :: IO ()
main = do
    input <- readInputFile "day23.txt"
    let inputs = lines input
    let graph = buildGraphFromLines inputs
    let allPaths = concatMap (\node -> findCycles node 2 graph) (nodeMatches "t*" graph)
    let p1 = length $ normalizePaths allPaths
    print "Part 1:"
    print p1
    let largestSubgroup = maximumClique graph
    let p2 = largestSubgroup
    print "Part 2:"
    print $ intercalate "," p2