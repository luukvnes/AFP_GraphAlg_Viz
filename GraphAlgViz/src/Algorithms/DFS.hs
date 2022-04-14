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


---------------------DFS-------------------------------------------------------
type DFSParams a = ((a -> Bool), [LNode (a,Flag)])

dfsStep :: Eq a => Ord a => AlgStep (a, Flag) b (DFSParams a) (Maybe (LNode a))
-- dfs has params
-- (LNode a -> Bool), a function that checks if the found node is the one were looking for
-- [LNode (a,Bool)], a stack
-- returns the node if it finds one.
dfsStep = Step dfsStep'

-- Change labeling to label as explored when dequeued for nicer visualization.
dfsStep' :: (Eq a) => Ord a =>
        DFSParams a ->
        Gr (a, Flag) b ->
        Either (Maybe (LNode a)) (Gr (a, Flag) b, DFSParams a)
dfsStep' (_, []) _ = Left Nothing
dfsStep' (p,q@(_,(label', _)):qs) graph | p label' = Left . Just . removeFlag $ q
                                | otherwise        = Right (newGraph, newParams)
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

-- runs the dfs algorithm by calling run with the right parameters
dfsRun :: Eq a => Ord a => (a -> Bool) -> Gr a b -> Maybe (LNode a)
dfsRun p graph = run dfsStep params flaggedGraph
  where params = (p, [addFlag (const Queued) firstNode])
        flaggedGraph = nmap (\x -> if Just x == lab graph (fst firstNode) then (x,Queued) else (x,Unexplored)) graph
        firstNode = head . labNodes $ graph

dfsStart :: Eq a => a -> Gr a b -> (DFSParams a, Gr (a, Flag) b)
dfsStart = bfsStart

