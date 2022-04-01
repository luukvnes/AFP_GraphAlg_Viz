module Algorithms where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree

import Data.Bifunctor
import Data.List
import qualified Data.Map as M

import Graph
import Helper

newtype AlgStep a b p r = Step {step :: p -> Gr a b -> Either r (Gr a b, p)}
{- Parameterized by
    a, the type of the label of the node
    b, the type of the label of the edge
    p, the paramters used in the algorithm
    r, the result type, eg a Path (from start to end node) for bfs
-}

-- Creates a new step function that alters the result using f
-- If there is no result f does nothing.
instance Functor (AlgStep a b p) where
  fmap f (Step alg) = Step $ \params graph -> first f $ alg params graph

instance Applicative (AlgStep a b p) where
  pure r = Step (\_ _ -> Left r)
  -- If both return a result, then apply the function to the result.
  (Step f) <*> (Step x) = Step $ \p g -> case (f p g, x p g) of
                                              (Left function, Left result) -> Left $ function result
                                              (Left function, Right x) -> Right x
                                              (Right x, Left result) -> Right x
                                              (Right x, Right y) -> Right x


--executes step until it reaches a result.
run :: AlgStep a b p r -> p -> Gr a b -> r
run algStep params graph = case step algStep params graph of
                                      Left r -> r
                                      Right (newGraph, newParams) -> run algStep newParams newGraph


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

type BFSParams a = ((LNode a -> Bool), [LNode (a,Flag)])

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
bfsStep' (p, []) graph = Left Nothing
bfsStep' (p, (q@(n,l'):qs)) graph | p . removeFlag $ q = Left . Just . removeFlag $ q
                                  | otherwise          = Right (newGraph, newParams)
  where --the new graph is the old graph where the labels have been updated accoring to if the nodes have been explored.
        newGraph = nmap f graph
        f l@(label, Queued)     = if l == l' then l else (label, Explored)
        f l@(label, Unexplored) = if l == l' then (label, Queued) else l
        f l = l
        --the new parameters are the same as the old ones, only the queue is appended with unexplored nodes, now marked explored
        newParams = (p, qs ++ unexploredNodes)
        --get all outgoing neighbours of the first node in the queue en check if they have been explored by inspecting their flag
        unexploredNodes = filter (\x -> getFlag x == Unexplored) $ listOutNeighbors graph $ fst q


-- runs the bfs algorithm by calling run with the right parameters
bfsRun :: Eq a => (LNode a -> Bool) -> Gr a b -> Maybe (LNode a)
bfsRun p graph = run bfsStep params flaggedGraph
  where params = (p, [addFlag (const Queued) firstNode])
        flaggedGraph = nmap (\x -> if (Just x == lab graph (fst firstNode)) then (x,Queued) else (x,Unexplored)) graph
        firstNode = head . labNodes $ graph




---------------------DFS-------------------------------------------------------
type DFSParams a = ((LNode a -> Bool), [LNode (a,Flag)])

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
dfsStep' (p, []) graph = Left Nothing
dfsStep' (p, q@(n,l'):qs) graph | p . removeFlag $ q = Left . Just . removeFlag $ q
                           | otherwise          = Right (newGraph, newParams)
  where --the new graph is the old graph where the labels have been updated accoring to if the nodes have been explored.
        newGraph = nmap f graph
        f l@(label, Queued)     = if l == l' then l else (label, Explored)
        f l@(label, Unexplored) = if l == l' then (label, Queued) else l
        f l = l
        --the new parameters are the same as the old ones, only the queue is appended with unexplored nodes, now marked explored
        newParams = (p, unexploredNodes ++ qs)
        --get all outgoing neighbours of the first node in the queue en check if they have been explored by inspecting their flag
        unexploredNodes = sortOn fst $ filter (\x -> getFlag x == Unexplored) $ listOutNeighbors graph $ fst q


-- runs the dfs algorithm by calling run with the right parameters
dfsRun :: Eq a => Ord a => (LNode a -> Bool) -> Gr a b -> Maybe (LNode a)
dfsRun p graph = run dfsStep params flaggedGraph
  where params = (p, [addFlag (const Queued) firstNode])
        flaggedGraph = nmap (\x -> if Just x == lab graph (fst firstNode) then (x,Queued) else (x,Unexplored)) graph
        firstNode = head . labNodes $ graph




------------------Dijkstra--------------------------------------------

{-
1  function Dijkstra(Graph, source):
2
3      for each vertex v in Graph.Vertices:
4          dist[v] ← INFINITY
5          prev[v] ← UNDEFINED
6          add v to Q
7      dist[source] ← 0
8
9      while Q is not empty:
10          u ← vertex in Q with min dist[u]
11          remove u from Q
12
13          for each neighbor v of u still in Q:
14              alt ← dist[u] + Graph.Edges(u, v)
15              if alt < dist[v]:
16                  dist[v] ← alt
17                  prev[v] ← u
18
19      return dist[], prev[]
-}

type DijkParams a = (Q a, DistList a)

type DistList a = M.Map (LNode a) (Int, LNode a)
type Q a = [LNode a]

dijkStep :: Eq a => AlgStep a b (DijkParams a) (DistList a)
dijkStep = Step dijkStep'

dijkStep' :: Eq a => DijkParams a -> Gr a b -> Either (DistList a) (Gr a b, DijkParams a)
--if the queue is empty, return the distance and preveous lists.
dijkStep' ([], dists) graph = Left dists
dijkStep' (q, dists) graph = Right (graph, newParams)
  where --find the node with minimal distance in the queue
        minDistNode' = foldr1 minTuple . filter ((flip elem) q . fst) . M.toList $ dists
        minTuple x@(_,(d,_)) y@(_,(d',_)) = if d < d' then x else y
        --extract the actual node and the distance
        minDistNode = fst minDistNode'
        minDist = fst . snd $ minDistNode'
        --get all the neighbours in the queue
        neighbors = filter ((flip elem) q) $ listNeighbors graph $ fst minDistNode
        --check if there is a shorter path using f and update accordingly
        newDists = M.mapWithKey f dists
        f n (i,x) = if p n (i,x) then (1 + minDist, minDistNode) else (i,x)
        p n (i,x) = n `elem` neighbors && (i > (1 + minDist))
        newParams = (delete minDistNode q, newDists)


dijkRun :: Eq a => LNode a -> Gr a b -> DistList a
dijkRun n graph = run dijkStep params graph
  where params = undefined
