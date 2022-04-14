module Algorithms where
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances  #-}


import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree

import Data.Bifunctor
import Data.List
import qualified Data.Map as M

import Graph
import Helper
import GHC.Base (undefined)

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

instance (Eq a) => Eq ((LNode a -> Bool)) where
        a == b = True
instance (Show a) => Show ((LNode a -> Bool)) where
        show a = "f"

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
bfsStep' (_, []) _ = Left Nothing
bfsStep' (p, (q@(_,(label', _)):qs)) graph | p . removeFlag $ q = Left . Just . removeFlag $ q
                                  | otherwise          = Right (newGraph, newParams)
  where --the new graph is the old graph where the labels have been updated accoring to if the nodes have been explored.
        newGraph = nmap f graph
        f l@(label, Queued)     = if label == label' then (label, Explored) else l
        f l@(label, Unexplored) = if label `elem` (map (fst.snd) unexploredNodes) then (label, Queued) else l
        f l = l
        --the new parameters are the same as the old ones, only the queue is appended with unexplored nodes, now marked explored
        newParams = (p, qs ++ unexploredNodes)
        --get all outgoing neighbours of the first node in the queue en check if they have been explored by inspecting their flag
        unexploredNodes = sortOn fst $ filter (\x -> getFlag x == Unexplored) $ listOutNeighbors graph $ fst q


-- runs the bfs algorithm by calling run with the right parameters
bfsRun :: Eq a => (LNode a -> Bool) -> Gr a b -> Maybe (LNode a)
bfsRun p graph = run bfsStep params flaggedGraph
  where params = (p, [addFlag (const Queued) firstNode])
        flaggedGraph = nmap (\x -> if (Just x == lab graph (fst firstNode)) then (x,Queued) else (x,Unexplored)) graph
        firstNode = head . labNodes $ graph


bfsStart :: Eq a => Gr a b -> (BFSParams a, Gr (a, Flag) b)
bfsStart graph = (params, flaggedGraph)
  where
    firstNode = head . labNodes $ graph
    flaggedGraph = nmap (\x -> if (Just x == lab graph (fst firstNode)) then (x,Queued) else (x,Unexplored)) graph
    p n@(i,l) = False
    params = (p, [addFlag (const Queued) firstNode])


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
dfsStep' (_, []) _ = Left Nothing
dfsStep' (p,q@(_,(label', _)):qs) graph | p . removeFlag $ q = Left . Just . removeFlag $ q
                                | otherwise          = Right (newGraph, newParams)
  where --the new graph is the old graph where the labels have been updated accoring to if the nodes have been explored.
        newGraph = nmap f graph
        f l@(label, Queued)     = if label == label' then (label, Explored) else l
        f l@(label, Unexplored) = if label `elem` map (fst.snd) unexploredNodes then (label, Queued) else l
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

dfsStart :: Eq a => Gr a b -> (DFSParams a, Gr (a, Flag) b)
dfsStart = bfsStart



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

--parameters passed around are a queue and a distance list
type DijkParams a = (Q a, DistList a)

--distance list is a dictionary storing a node, together wits its distaance to the source and the preveous node on the route
type DistList a = M.Map (LNode (a, Flag)) (Int, LNode (a, Flag))
type Q a = [LNode (a, Flag)]

dijkStep :: Eq a => AlgStep (a, Flag) b (DijkParams a) [(LNode a, Int, LNode a)]
dijkStep = fmap f $ Step dijkStep'
  --map f to format the output a little bit
  where f = map (\(node, (dist, prev)) -> (removeFlag node, dist, removeFlag prev)) . M.toAscList

dijkStep' :: Eq a => DijkParams a -> Gr (a, Flag) b -> Either (DistList a) (Gr (a, Flag) b, DijkParams a)
--if the queue is empty, return the distance and preveous lists.
dijkStep' ([], dists) _ = Left dists
dijkStep' (q, dists) graph = Right (newGraph, newParams)
  where --find the node with minimal distance in the queue
        minDistInfo = foldr1 minTuple . filter ((flip elem) q . fst) . M.toList $ dists
        minTuple x@(_,(d,_)) y@(_,(d',_)) = if d < d' then x else y
        --extract the actual node and the distance
        minDistNode = fst minDistInfo
        minDist = fst . snd $ minDistInfo
        --get all the neighbours in the queue
        neighbors = filter ((flip elem) q) $ listOutNeighbors graph $ fst minDistNode
        --check if there is a shorter path using f and update accordingly
        --use of `listNeighbors` instead of `listOutNeighbours` makes this algorithm undirected.
        newDists = M.mapWithKey f dists
        f n (i,x) = if p n (i,x) then (1 + minDist, minDistNode) else (i,x)
        p n (i,_) = n `elem` neighbors && (i > (1 + minDist))
        newParams = (delete minDistNode q, newDists)
        -- update flags. If the node is queued then mark it explored. Mark the next node as queued and keep all other labels the same.
        newGraph = nmap updateFlag graph
        updateFlag l | l == snd minDistNode = (fst l, Queued) --set the node to queued if it is the current node
        updateFlag l | snd l == Queued      = (fst l, Explored) --if the node was preveously queued, set it to explored
        updateFlag l | otherwise            = (fst l, snd l) --otherwise, keep the same flag.




dijkRun :: (Eq a, Ord a) => LNode a -> Gr a b -> [(LNode a, Int, LNode a)]
dijkRun (i,l) graph = run dijkStep params flaggedGraph
  where params             = (q, dists)
        -- initial queue consists of all nodes
        q                  = listNodes flaggedGraph
        -- distances are unexplored, except for the source node.
        dists              = M.mapWithKey f $ M.fromList $ zip q (repeat (maxBound, (i,(l,Unexplored))))
        f (i',(l',Queued)) _ = (0, (i',(l',Unexplored)))
        f _              v = v
        flaggedGraph       = nmap (\x -> if x == l then (x,Queued) else (x,Unexplored)) graph


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
-- type SCCParams a = (StepNumber, )


-- | 'SCCParams' contain the paramaters for all the SCC steps. There are three constructors, 
data SCCParams a =
        -- | 'SOne' takes the paramaters for the first step, an int that counts how many nodes have been added to the stack, the scc stack and the current path of the dfs as stack.
        SOne Int (S (SCCFlagNode a)) (S (SCCFlagNode a)) |
        -- | 'STwo' takes the paramaters for the second step, it only takes the stack, which it does not use except to pass on to the third step
        STwo (S (SCCFlagNode a)) |
        -- | 'SThree' takes the paramaters for the third step, an int that keeps track of which component is being discovered, the scc stack, and dfs stack.
        SThree Int (S (SCCFlagNode a)) (S (SCCFlagNode a))
        deriving (Eq, Show)

-- | A type signature for a stack
type S a = [LNode a]
-- | The type of the nodes used in scc
type SCCFlagNode a = (a, Flag, Int)

-- | A step function for the strongly connected component algorithm
sccStep ::  (Eq a, Ord a, Show a) => AlgStep (SCCFlagNode a) b (SCCParams a) (Int)
sccStep = Step sccStep'

-- | A step function for the strongly connected component algorithm
sccStep' ::  (Eq a, Ord a, Show a) => SCCParams a -> Gr (SCCFlagNode a) b -> Either Int (Gr (SCCFlagNode a) b, SCCParams a)
sccStep' (SOne sccID a b) graph = Right (sccStep1' (sccID,a, b) graph)
sccStep' (STwo stack) graph = Right (sccStep2' stack graph)
sccStep' (SThree sccID a b) graph = sccStep3' (sccID,a, b) graph

-- | A step function for the first step of the strongly connected component algorithm
-- This step does DFS (restarting at unexplored vertices) adding vertices to a stack whenever they have no unexplored vertices
-- If this step is finished it produces the paramaters for the second step, otherwise it produces another set of step 1 parameters.
sccStep1' :: (Eq a, Ord a, Show a) => (Int, S (SCCFlagNode a), S (SCCFlagNode a)) -> Gr (SCCFlagNode a) b -> (Gr (SCCFlagNode a) b, SCCParams a)
sccStep1' (sccID, ss, oldPath@(p1@(n,(label', _, _)):p2:ps)) graph = (newGraph, newParams)
    where
        newGraph = nmap f graph
        f l@(label, Queued, _)     = if label == label' && null unexploredNodes then (label, Explored, sccID) else l
        f l@(label, Unexplored, _) = if not (null unexploredNodes) && fstT (snd (head unexploredNodes)) ==  label then (label, Queued, -1)  else l
        f l = l
        
        newParams = if null unexploredNodes
            then SOne (sccID +1) (p1:ss) (p2:ps)
            else SOne sccID ss (head unexploredNodes:oldPath)
        unexploredNodes = sortOn fst $ filter (\x -> getFlagSCC x == Unexplored) $ listOutNeighbors graph n
-- if path is empty try to find new starting node, if not found go to step 2
sccStep1' (sccID, ss, [p1@(n,(label', _, _))]) graph = (newGraph, newParams)
    where
        newGraph = nmap f graph
        f l@(label, Queued, _)     = if label == label' && null unexploredNodes then (label, Explored, sccID) else l
        f l@(label, Unexplored, _) = if not (null unexploredNodes) && fstT (snd (head unexploredNodes)) ==  label then (label, Queued, -1)  else l
        f l = l
        newParams = if null unexploredNodes
            then STwo (p1:ss) -- third and fourth params dont matter in this case
            else SOne sccID ss (head unexploredNodes:[p1])
        unexploredNodes = sortOn fst $ filter (\x -> getFlagSCC x == Unexplored) $ listOutNeighbors graph n
sccStep1' _ _ = error "this should not happen"

-- | A step function for the second step of the strongly connected component algorithm
-- This step reverses all the edges in the graph
sccStep2' ::  (Eq a, Ord a, Show a) => S (SCCFlagNode a) -> Gr (SCCFlagNode a) b -> (Gr (SCCFlagNode a) b, SCCParams a)
sccStep2' (s:ss) graph = (createFlaggedGraph newGraph firstNode, newParams)
    where
        newGraph = fromEdgeList newEdgeList
        newEdgeList = map (labelEdge graph) (labEdges graph)
        firstNode = removeFlagSCC s
        newParams = SThree 5 (s:ss) [addFlagSCC (const Queued) firstNode]
sccStep2' _ _ = error "empty graph"

-- | A step function for the third step of the strongly connected component algorithm
-- This step does DFS restarting at the front node of the stack, every node it encounters belongs to the component of the starting node
sccStep3' ::  (Eq a, Ord a, Show a) => (Int, S (SCCFlagNode a), S (SCCFlagNode a)) -> Gr (SCCFlagNode a) b -> Either Int (Gr (SCCFlagNode a) b, SCCParams a)
sccStep3' (sccID, ss, (n,(label',_,_)):qs) graph = Right (newGraph, newParams)
    where
        newGraph = nmap f graph
        f l@(label, Queued,_)     = if label == label' then (label, Explored, sccID) else l
        f l@(label, Unexplored,_) = if label `elem` map (fstT.snd) unexploredNodes then (label, Queued,-1) else l
        f l = l
        -- TODO remove l' from stack
        newParams = SThree sccID (removeFromStack ss) (unexploredNodes ++ qs)
        removeFromStack [] = []
        removeFromStack (x@(n',_):stack)
                | n' == n = stack
                | otherwise = x : removeFromStack stack
        unexploredNodes = sortOn fst $ filter (\x -> getFlagSCC x == Unexplored) $ listOutNeighbors graph n


-- Find new starting location
sccStep3' (sccID, (n,(label', _,_)):ss, []) graph = Right (newGraph, newParams)
    where
        newGraph = nmap f graph
        f l@(label, Queued,_)     = if label == label' then l else (label, Explored,sccID)
        f l@(label, Unexplored,_) = if label == label' then (label, Queued,sccID+1) else l
        f l = l
        -- newParams :: SCCParams a
        newParams = SThree (sccID+1) ss [(n,(label', Queued, sccID+1))]
sccStep3' (_, [], []) _ = Left 1

-- for some reason putting this in the where clause does not work
createFlaggedGraph :: Eq a => Gr a b -> (Node, a) -> Gr (SCCFlagNode a) b
createFlaggedGraph graph firstNode = nmap (\x -> if (Just x == lab graph (fst firstNode)) then (x,Queued, -1) else (x,Unexplored, -1)) graph

-- | given a graph and an edge transform it to an edgelist value
labelEdge :: Eq a => Gr (SCCFlagNode a) b -> (Node, Node, b) -> (a, a, b)
labelEdge graph (from, to, label) = case lab graph from of
        Just x -> case lab graph to of
                Just y -> (fstT y, fstT x, label)
                Nothing -> error "no label found"
        Nothing -> error "no label found"
        
-- | Produces the starting paramaters for scc with the gives graph
sccStart :: Eq a => Gr a b -> (SCCParams a, Gr (a, Flag, Int) b)
sccStart graph = (params, flaggedGraph)
  where
    firstNode = head . labNodes $ graph
    flaggedGraph = nmap (\x -> if Just x == lab graph (fst firstNode) then (x,Queued,-1) else (x,Unexplored,-1)) graph
    params = SOne 0 [] [addFlagSCC (const Queued) firstNode]