{-# OPTIONS_GHC -F -pgmF htfpp #-}

module GraphTest (htf_thisModulesTests) where

import Test.Framework

import qualified Data.Map as Map
import qualified Data.List as List
import Graph

-- Generators

-- Generates a arbitrary graph
genGraph :: Gen (Graph Int)
genGraph = do
  nodes <- arbitrary
  adjacentLists <- vectorOf (length nodes) $ listOf $ elements $ nodes
  return $ Map.fromList $ zip nodes adjacentLists

-- Generates a complete graph, i.e., a graph where a all distinct node pairs
-- are connected by a unique edge.
genCompleteGraph :: Gen (Graph Int)
genCompleteGraph = do
  nodes <- List.nub <$> arbitrary
  return
    $ Map.fromList
    $ map (\node -> (node, List.delete node nodes)) nodes

-- Generate a graph with zero edges.
genFullyDisconnectedGraph :: Gen (Graph Int)
genFullyDisconnectedGraph = do
  nodes <- arbitrary
  return $ Map.fromList $ map (\node -> (node, [])) nodes

-- Transposing a graph twice should be equal to the same graph
-- i.e. (G^T)^T = G.
transpose_transpose :: Ord a => Graph a -> Bool
transpose_transpose graph =
  Map.map List.sort (transpose . transpose $ graph) == Map.map List.sort graph

prop_transpose_transpose = forAll genGraph transpose_transpose

prop_sourceNodesZeroCompleteGraph =
  forAll genCompleteGraph $ \graph ->
    (length $ sourceNodes graph) == sourceNodeCount graph
  where sourceNodeCount graph
          | Map.size graph /= 1 = 0
          | otherwise = 1

prop_sourceNodesAllFullyDisonnectedGraph =
  forAll genFullyDisconnectedGraph $ \graph ->
    (length $ sourceNodes graph) == Map.size graph
