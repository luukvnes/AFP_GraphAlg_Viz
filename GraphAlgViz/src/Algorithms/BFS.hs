module Algorithms.BFS where 

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree

import Algorithms

import Data.Bifunctor
import Data.List
import qualified Data.Map as M

import Graph
import Helper
import GHC.Base (undefined)


type BFSParams a = ((a -> Bool), [LNode (a,Flag)])


bfsStep :: Eq a => AlgStep (a, Flag) b (BFSParams a) (Maybe (LNode a))
-- bfs has params
-- (LNode a -> Bool), a function that checks if the found node is the one were looking for
-- [LNode (a,Bool)], a queue
-- returns the node if it finds one.
bfsStep = Step bfsStep'

{-
--Original labeling as in Psuedocode
bfsStep' :: (Eq a) =>
        BFSParams a ->
        Gr (a, Bool) b ->
        Either (Maybe (LNode a)) (Gr (a, Bool) b, BFSParams a)
bfsStep' (p, []) graph = Left Nothing
bfsStep' (p, (q:qs)) graph | p . removeFlag $ q = Left . Just . removeFlag $ q
                           | otherwise          = Right (newGraph, newParams)
  where --the new graph is the old graph where the labels have been updated accoring to if the nodes have been explored.
        newGraph = nmap f graph
        f (label, True) = (label, True)
        f l@(label, False) = if (Just l) `elem` (map (\x -> lab graph (fst x)) unexploredNodes) then (label, True) else l
        --the new parameters are the same as the old ones, only the queue is appended with unexplored nodes, now marked explored
        newParams = (p, qs ++ map (setFlag (const True)) unexploredNodes)
        --get all outgoing neighbours of the first node in the queue en check if they have been explored by inspecting their flag
        unexploredNodes = filter (\x -> getFlag x == False) $ listOutNeighbors graph $ fst q
-}

-- Change labeling to label as explored when dequeued for nicer visualization.
bfsStep' :: (Eq a) =>
        BFSParams a ->
        Gr (a, Flag) b ->
        Either (Maybe (LNode a)) (Gr (a, Flag) b, BFSParams a)
bfsStep' (_, []) _                     = Left Nothing
bfsStep' (p, (q:qs)) graph | p label'  = Left . Just . removeFlag $ q
                           | otherwise = Right (newGraph, newParams)
  where label' = fst . snd $ q
        --the new graph is the old graph where the labels have been updated accoring to if the nodes have been explored.
        newGraph = nmap f graph
        f l@(label, Queued)     | label == label' = (label, Explored)
        f l@(label, Unexplored) | label `elem` (map (fst.snd) unexploredNodes) = (label, Queued)
        f l                     | otherwise = l
        --the new parameters are the same as the old ones, only the queue is appended with unexplored nodes, now marked explored
        newParams = (p, qs ++ unexploredNodes)
        --get all outgoing neighbours of the first node in the queue en check if they have been explored by inspecting their flag
        unexploredNodes = sortOn fst $ filter unexplored $ listOutNeighbors graph $ fst q
        unexplored n = let flag = getFlag n in flag == Unexplored || flag == Goal


-- runs the bfs algorithm by calling run with the right parameters
bfsRun :: Eq a => (a -> Bool) -> Gr a b -> Maybe (LNode a)
bfsRun p graph = run bfsStep params flaggedGraph
  where params = (p, [addFlag (const Queued) firstNode])
        flaggedGraph = nmap flagNode graph
        flagNode l | p l = (l, Goal)
        flagNode l | Just l == lab graph (fst firstNode) = (l,Queued)
        flagNode l | otherwise = (l,Unexplored)
        firstNode = head . labNodes $ graph


bfsStart :: Eq a => a -> Gr a b -> (BFSParams a, Gr (a, Flag) b)
bfsStart target graph = (params, flaggedGraph)
  where
    firstNode = head . labNodes $ graph
    flaggedGraph = nmap flagNode graph
    flagNode l | l == target = (l, Goal)
    flagNode l | Just l == lab graph (fst firstNode) = (l,Queued)
    flagNode l | otherwise = (l,Unexplored)
    p l = l == target
    params = (p, [addFlag (const Queued) firstNode])


--------------BFS----------------------------------------

{-
Psuedocode for the implementation of breadth first search
1  procedure BFS(G, root) is
2      let Q be a queue
3      label root as explored
4      Q.enqueue(root)
5      while Q is not empty do
6          v := Q.dequeue()
7          if v is the goal then
8              return v
9          for all edges from v to w in G.adjacentEdges(v) do
10              if w is not labeled as explored then
11                  label w as explored
12                  Q.enqueue(w)
-}
