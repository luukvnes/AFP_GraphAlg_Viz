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


------------------SCC--------------------------------------------

{-
1  function SCC(Graph, source):
2
3      DFS (restarting at unexplored vertices) adding vertices to a stack whenever they have no unexplored vertices
4      reverse all edge directions
5      DFS restarting at unexplored vertex top of the stack, every vertex you discover from a single starting vertex belongs to scc 
-}


-- step1, id of when node was put in stack, scc stack, current path (as a stack), next node,
-- step2, _
-- step3, current component ID, scc stack, dfs stack, next node
-- Step, stack, next node
type SCCParams a = (Int, Int, S (SCCFlagNode a), S (SCCFlagNode a), LNode (SCCFlagNode a))

type S a = [LNode a]
type SCCFlagNode a = (a, Flag, Int)

sccStep :: Eq a =>  Ord a => AlgStep (SCCFlagNode a) b (SCCParams a) (Int)
sccStep = Step sccStep'

sccStep' :: Eq a => Ord a => SCCParams a -> Gr (SCCFlagNode a) b -> Either (Int) (Gr (SCCFlagNode a) b, SCCParams a)
sccStep' (1, sccID, a, b, c) graph = Right ( sccStep1' (sccID,a, b, c) graph)
sccStep' (2, _, stack, _, _) graph = Right (sccStep2' stack graph)
sccStep' (3, sccID, a, b, _) graph = sccStep3' (sccID,a, b) graph


sccStep1' :: Eq a => Ord a => (Int, S (SCCFlagNode a), S (SCCFlagNode a), LNode (SCCFlagNode a)) -> Gr (SCCFlagNode a) b -> (Gr (SCCFlagNode a) b, SCCParams a)
sccStep1' (sccID, ss, oldPath@(p1:p2:ps), (n,l')) graph = (newGraph, newParams)
    where
        newGraph = nmap f graph
        f l@(label, Queued, _)     = if l == l' && null unexploredNodes then l else (label, Explored, sccID)
        f l@(label, Unexplored, _) = if l == l' then (label, Queued, -1) else l
        f l = l
        -- newParams :: SCCParams a
        newParams = if null unexploredNodes
            then (1, sccID +1, p1:ss, p2:ps, p2)
            else (1, sccID, ss, head unexploredNodes:oldPath, head unexploredNodes)
        -- unexploredNodes :: [LNode (a, Flag)]
        unexploredNodes = sortOn fst $ filter (\x -> getFlagSCC x == Unexplored) $ listOutNeighbors graph n
-- if path is empty try to find new starting node, if not found go to step 2
sccStep1' (sccID, ss, [p1], (n,l')) graph = (newGraph, newParams)
    where
        newGraph = nmap f graph
        f l@(label, Queued, _)     = if l == l' && null unexploredNodes then l else (label, Explored, sccID)
        f l@(label, Unexplored, _) = if l == l' then (label, Queued, -1) else l
        f l = l
        -- newParams :: SCCParams a
        newParams = if null unexploredNodes
            then (2, sccID+1, p1:ss, [], p1) -- third and fourth params dont matter in this case
            else (1, sccID, ss, head unexploredNodes:[p1], head unexploredNodes)
        -- unexploredNodes :: [LNode (a, Flag)]
        unexploredNodes = sortOn fst $ filter (\x -> getFlagSCC x == Unexplored) $ listOutNeighbors graph n

sccStep2' :: Eq a => Ord a => S (SCCFlagNode a) -> Gr (SCCFlagNode a) b -> (Gr (SCCFlagNode a) b, SCCParams a)
sccStep2' (s:ss) graph = (createFlaggedGraph newGraph firstNode, newParams)
    where
        newGraph = fromEdgeList newEdgeList
        -- flaggedGraph = nmap (\x -> if (Just x == lab graph (fst firstNode)) then (x,Queued) else (x,Unexplored)) newGraph
        newEdgeList = map (labelEdge graph) (labEdges graph)
        firstNode = removeFlagSCC s
        newParams = (3, 0, s:ss, [addFlagSCC (const Queued) firstNode], addFlagSCC (const Queued) firstNode)

sccStep3' :: Eq a => Ord a => (Int, S (SCCFlagNode a), S (SCCFlagNode a)) -> Gr (SCCFlagNode a) b -> Either Int (Gr (SCCFlagNode a) b, SCCParams a)
sccStep3' (sccID, ss, q@(n,l'):qs) graph = Right (newGraph, newParams)
    where
        newGraph = nmap f graph
        f l@(label, Queued, _)     = if l == l' then l else (label, Explored,sccID)
        f l@(label, Unexplored, _) = if l == l' then (label, Queued,-1) else l
        f l = l
        -- TODO remove l' from stack
        newParams = (3, sccID, removeFromStack ss, unexploredNodes ++ qs,(n,l'))
        removeFromStack [] = []
        removeFromStack (x@(n',_):ss)
                | n' == n = ss
                | otherwise = x : removeFromStack ss
        unexploredNodes = sortOn fst $ filter (\x -> getFlagSCC x == Unexplored) $ listOutNeighbors graph n



sccStep3' (sccID, (n,l'@(label, _,_)):ss, []) graph = Right (newGraph, newParams)
    where
        newGraph = nmap f graph
        f l@(label, Queued,_)     = if l == l' then l else (label, Explored,sccID)
        f l@(label, Unexplored,_) = if l == l' then (label, Queued,sccID+1) else l
        f l = l
        -- newParams :: SCCParams a
        newParams = (3, sccID+1, ss, [(n,(label, Queued, sccID+1))], (n,l'))
sccStep3' (sccID, [], []) graph = Left 1

-- for some reason putting this in the where clause does not work
createFlaggedGraph :: Eq a => Gr a b -> (Node, a) -> Gr (SCCFlagNode a) b
createFlaggedGraph graph firstNode = nmap (\x -> if (Just x == lab graph (fst firstNode)) then (x,Queued, -1) else (x,Unexplored, -1)) graph

-- given a graph and an edge transform it to an edgelist value
labelEdge :: Eq a => Gr (SCCFlagNode a) b -> (Node, Node, b) -> (a, a, b)
labelEdge graph (from, to, label) = case lab graph from of
        Just x -> case lab graph to of
                Just y -> (fstT y, fstT x, label)
                Nothing -> error "no label found"
        Nothing -> error "no label found"
