{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graph 
  ( Graph
  , empty
  , addNode
  , addNodes
  , connect
  , getNodes
  , getConnections
  , hasConnection
  , getNodesWithinDegrees
  , normalizePaths
  , findCycles
  , nodeMatches
  , bronKerbosch
  , maximumClique
  ) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.List (maximumBy, minimumBy, foldl')
import Data.Ord (comparing)

data Graph a = Graph 
  { nodes :: Set.Set a                    
  , connections :: Map.Map a (Set.Set a) 
  } deriving (Show, Eq)

empty :: (Ord a) => Graph a
empty = Graph Set.empty Map.empty

addNode :: (Ord a) => a -> Graph a -> Graph a
addNode node graph = Graph
  { nodes = Set.insert node (nodes graph)
  , connections = connections graph
  }

addNodes :: (Ord a) => [a] -> Graph a -> Graph a
addNodes newNodes graph = Graph
  { nodes = foldr Set.insert (nodes graph) newNodes
  , connections = connections graph
  }

connect :: (Ord a) => a -> a -> Graph a -> Graph a
connect node1 node2 graph
  | node1 == node2 = graph
  | otherwise = Graph
      { nodes = Set.insert node1 $ Set.insert node2 $ nodes graph
      , connections = updatedConnections
      }
  where
    updatedConnections = Map.insert node2 (addConnection node1 node2Connections) $
                        Map.insert node1 (addConnection node2 node1Connections) $
                        connections graph
    node1Connections = fromMaybe Set.empty $ Map.lookup node1 $ connections graph
    node2Connections = fromMaybe Set.empty $ Map.lookup node2 $ connections graph
    addConnection node conns = Set.insert node conns

getNodes :: Graph a -> Set.Set a
getNodes = nodes

getConnections :: (Ord a) => a -> Graph a -> Set.Set a
getConnections node graph = fromMaybe Set.empty $ Map.lookup node $ connections graph

hasConnection :: (Ord a) => a -> a -> Graph a -> Bool
hasConnection node1 node2 graph = 
  Set.member node2 $ fromMaybe Set.empty $ Map.lookup node1 $ connections graph

getNodesWithinDegrees :: forall a. (Ord a) => a -> Int -> Graph a -> [[a]]
getNodesWithinDegrees startNode degrees graph
  | degrees < 0 = []
  | degrees == 0 = [[startNode]]
  | otherwise = go degrees [[startNode]]
  where
    go :: Int -> [[a]] -> [[a]]
    go 0 paths = paths
    go n currentPaths
      | null currentPaths = []
      | otherwise =
          let nextPaths = [ path ++ [next] 
                         | path <- currentPaths
                         , let lastNode = last path
                         , next <- Set.toList $ getConnections lastNode graph
                         , next `notElem` path 
                         ]
          in go (n-1) nextPaths

findCycles :: Ord a => a -> Int -> Graph a -> [[a]]
findCycles startNode degrees graph = 
  filter (hasCycle graph) $ getNodesWithinDegrees startNode degrees graph

normalizePaths :: (Ord a) => [[a]] -> [[a]]
normalizePaths = map head . Map.elems . Map.fromListWith (++) . map (\path -> (Set.fromList path, [path]))

hasCycle :: Ord a => Graph a -> [a] -> Bool
hasCycle graph nodes 
  | length nodes < 3 = False
  | otherwise = all isFullyConnected nodes
  where
    isFullyConnected node = 
      let connections = getConnections node graph
          otherNodes = Set.fromList $ filter (/= node) nodes
      in otherNodes `Set.isSubsetOf` connections

nodeMatches :: String -> Graph String -> [String]
nodeMatches pattern graph = 
    let patternText = T.pack pattern
        starIndex = T.findIndex (== '*') patternText
        nodeList = Set.toList $ nodes graph
    in case starIndex of
        Nothing -> 
            filter (\n -> T.pack n == patternText) nodeList
        Just idx -> 
            let prefix = T.take idx patternText
            in filter (\n -> 
                let nodeStr = T.pack n
                in T.isPrefixOf prefix nodeStr) nodeList

bronKerbosch :: Ord a => Graph a -> Set.Set a -> Set.Set a -> Set.Set a -> [[a]]
bronKerbosch graph r p x
    | Set.null p && Set.null x = [Set.toList r]
    | Set.null p = []
    | otherwise = 
        let pivot = choosePivot graph p x
            pivotNeighbors = getConnections pivot graph
            verticesToProcess = Set.difference p pivotNeighbors
        in concatMap (\v -> 
            let vNeighbors = getConnections v graph
            in bronKerbosch graph 
                           (Set.insert v r)
                           (Set.intersection p vNeighbors)
                           (Set.intersection x vNeighbors)
           ) (Set.toList verticesToProcess)

maximumClique :: Ord a => Graph a -> [a]
maximumClique graph = 
    let orderedNodes = getDegeneracyOrder graph
        results = foldl' (\cliques v ->
            let vNeighbors = getConnections v graph
                laterNodes = Set.fromList $ dropWhile (/= v) orderedNodes
                p = Set.intersection vNeighbors laterNodes
                x = Set.intersection vNeighbors (Set.fromList $ takeWhile (/= v) orderedNodes)
            in cliques ++ bronKerbosch graph (Set.singleton v) p x
            ) [] orderedNodes
    in maximumBy (comparing length) results

choosePivot :: Ord a => Graph a -> Set.Set a -> Set.Set a -> a
choosePivot graph p x = 
    let unionPX = Set.union p x
        pivotCandidates = Set.union p x
    in maximumBy (comparing (\v -> 
        Set.size $ Set.intersection (getConnections v graph) unionPX)) 
        (Set.toList pivotCandidates)

getDegeneracyOrder :: Ord a => Graph a -> [a]
getDegeneracyOrder graph = go (getNodes graph) []
  where
    go remain ordered 
      | Set.null remain = ordered
      | otherwise = 
          let v = minimumBy (comparing (\v -> Set.size $ Set.intersection remain $ getConnections v graph)) 
                           (Set.toList remain)
          in go (Set.delete v remain) (v:ordered)