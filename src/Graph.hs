module Graph (
    createGraph
  , createGraph'
  , dependencyLayerSort
  , downstreamNodes
  , Graph
  , sourceNodes
  , transpose
  , removeNode
  , removeNodes
) where

import qualified Data.Map as Map
import qualified Data.List as List
import Data.Function ((&))

type Graph a = Map.Map a [a]
-- An alternative representation of a graph, mainly used
-- for transposing a graph.
type EdgeFormat a = ([a], [(a,a)])

transpose
  :: Ord a
  => Graph a
  -> Graph a
transpose = edgeFormatToGraph . reverseEdgeFormat . graphToEdgeFormat

createGraph
  :: Ord a
  => (b -> a)
  -> (b -> [b])
  -> [b]
  -> Graph a
createGraph nodeToKey nodeToAdjacentList = createGraph' nodeToKey (map nodeToKey . nodeToAdjacentList)

createGraph'
  :: Ord a
  => (b -> a)
  -> (b -> [a])
  -> [b]
  -> Graph a
createGraph' nodeToKey nodeToAdjacentList nodes =
  nodes
  & map (\node -> (nodeToKey node, nodeToAdjacentList node))
  & Map.fromList

graphToEdgeFormat
  :: Ord a
  => Graph a
  -> EdgeFormat a
graphToEdgeFormat graph = graph
  & Map.toAscList
  & concatMap (\(source,nodes) -> map (source,) nodes)
  & (,) (Map.keys graph)

edgeFormatToGraph
  :: Ord a
  => EdgeFormat a
  -> Graph a
edgeFormatToGraph (keys, edges) =
  edges
  & List.sortBy (\x y -> compare (fst x) (fst y))
  & List.groupBy (\x y -> fst x == fst y)
  & map (\edges -> (fst (head edges), map snd edges))
  & Map.fromList
  & flip Map.union (map (, []) keys & Map.fromList)

reverseEdgeFormat
  :: EdgeFormat a
  -> EdgeFormat a
reverseEdgeFormat edgeFormat =
  (fst edgeFormat, [(y,x) | (x,y) <- snd edgeFormat])

dependencyLayerSort
  :: Ord a
  => (b -> a)
  -> (b -> [b])
  -> [b]
  -> Maybe [[b]]
dependencyLayerSort nodeToKey nodeToAdjacentList nodes =
    createGraph nodeToKey nodeToAdjacentList nodes
    & topologicalLayerSort
    & (map (map (keyToNode Map.!))<$>)
  where
    keyToNode = nodes & map (\node -> (nodeToKey node, node)) & Map.fromList

-- A topological layer sort algorithm which is inspired by Kahn's algorithm.
-- If the graph is not a DAG, i.e. the graph contains cycles, the function
-- returns Nothing.
--
-- Implementation:
-- The algorithm calculates the DAG of the transposed DAG and then revereses
-- the layered nodes, which is equivalent to calculating the topological
-- sorting directly.
topologicalLayerSort
  :: Ord a
  => Graph a
  -> Maybe [[a]]
topologicalLayerSort graph = go [] graph & (reverse<$>)
  where
    go :: Ord a => [[a]] -> Graph a -> Maybe [[a]]
    go prevLayers graphReduced
      | Map.size graphReduced == 0 = Just prevLayers
      | otherwise =
        let sinks = sinkNodes graphReduced
            continue = go
              (prevLayers <> [sinks])
              (removeNodes sinks graphReduced)
        in if null sinks then Nothing else continue

-- Nodes with no ingoing edges
sourceNodes
  :: Ord a
  => Graph a
  -> [a]
sourceNodes = sinkNodes . transpose

-- Nodes with no outgoing edges
sinkNodes
  :: Ord a
  => Graph a
  -> [a]
sinkNodes graph = graph
  & Map.toAscList
  & filter ((==[]) . snd)
  & map fst

downstreamNodes
  :: Ord a
  => Eq a
  => a
  -> Graph a
  -> [a]
downstreamNodes node graph =
  let directDownstream = graph Map.! node
      graphWithoutNode = removeNode node graph
  in List.nub
    $ directDownstream
      <> concatMap (`downstreamNodes` graphWithoutNode) directDownstream

removeNodes
  :: Ord a
  => [a]
  -> Graph a
  -> Graph a
removeNodes nodes graph = foldr removeNode graph nodes

removeNode :: Ord a => a -> Graph a -> Graph a
removeNode node graph =
  let newGraph = Map.delete node graph
  in foldr (Map.update (Just . filter (/=node))) newGraph (Map.keys newGraph)
