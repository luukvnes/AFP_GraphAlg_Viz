module Algorithms.DFS where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree

import Algorithms
import Algorithms.BFS

import Data.Bifunctor
import Data.List
import qualified Data.Map as M

import Graph
import Helper
import GHC.Base (undefined)


-- | 'DFSParams' conatins the parameters needed to run Breadth First Search
--   It consists of a function dictating whether a Node label is the label is the target
--   and a queue of nodes
type DFSParams a = ((a -> Bool), [LNode (a,Flag)])

-- | 'dfsStep' is the single step implementation of Depth First Search
--   DFS tries to find a node that satisifies the property in the 'DFSParams'
--   If it manages to find one, it returns it.
--   It requires the node labels to have a flag of type 'Flag'
dfsStep :: Eq a => Ord a => AlgStep (a, Flag) b (DFSParams a) (Maybe (LNode a))
dfsStep = Step dfsStep'

dfsStep' :: (Eq a) => Ord a =>
        DFSParams a ->
        Gr (a, Flag) b ->
        Either (Maybe (LNode a)) (Gr (a, Flag) b, DFSParams a)
dfsStep' (_, []) _                                  = Left Nothing
dfsStep' (p,q@(_,(label', _)):qs) graph | p label'  = Left . Just . removeFlag $ q
                                        | otherwise = Right (newGraph, newParams)
  where --the new graph is the old graph where the labels have been updated accoring to if the nodes have been explored.
        newGraph = nmap f graph
        f l@(label, Queued)     | label == label' = (label, Explored)
        f l@(label, Unexplored) | label `elem` (map (fst.snd) unexploredNodes) = (label, Queued)
        f l                     | otherwise = l
        --the new parameters are the same as the old ones, only the queue is appended with unexplored nodes, now marked explored
        newParams = (p, unexploredNodes ++ qs)
        --get all outgoing neighbours of the first node in the queue en check if they have been explored by inspecting their flag
        unexploredNodes = sortOn fst $ filter unexplored $ listOutNeighbors graph $ fst q
        unexplored n = let flag = getFlag n in flag == Unexplored || flag == Goal

-- | 'dfsRun' runs the dfs algorithm by calling run with the initial parameters
dfsRun :: Eq a => Ord a => (a -> Bool) -> Gr a b -> Maybe (LNode a)
dfsRun p graph = run dfsStep params flaggedGraph
  where params = (p, [addFlag (const Queued) firstNode])

        flaggedGraph = nmap flagNode graph

        flagNode l | p l                                 = (l, Goal)
        flagNode l | Just l == lab graph (fst firstNode) = (l,Queued)
        flagNode l | otherwise                           = (l,Unexplored)

        firstNode = head . labNodes $ graph

-- | creates inital parameters for 'dfsStep' from a graph 'Gr'
--   and a target label of type 'a'
--   notice this is less general than the overall implementation
--   since we assume the function dictating which node we are looking for simply
--   matches the label.
dfsStart :: Eq a => a -> Gr a b -> (DFSParams a, Gr (a, Flag) b)
dfsStart = bfsStart
