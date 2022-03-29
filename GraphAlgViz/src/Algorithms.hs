module Algorithms where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Data.Bifunctor

import Graph
import Helper

newtype AlgStep a b p r = Step (p -> Gr a b -> Either r (Gr a b, p))
{- Parameterized by
    a, the type of the label of the node
    b, the type of the label of the edge
    p, the paramters used in the algorithm
    r, the result type, eg a Path (from start to end node) for bfs
-}

{-
-- fmap runs the step creates a new step which runs the step until it reaches a result, then maps the function over it.
-- step $ fmap id step == run step
instance Functor (AlgStep a b p) where
  fmap f step = Step $ \p g -> Left $ f $ run step p g

instance Applicative (AlgStep a b p) where
  pure r = Step (\_ _ -> Left r)
  (Step f) <*> (Step x) = undefined
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



-- executes a stingle step of the algorithm defined by AlgStep
step :: AlgStep a b p r -> p -> Gr a b -> Either r (Gr a b, p)
step (Step alg) params graph = alg params graph

--executes step until it reaches a result.
run :: AlgStep a b p r -> p -> Gr a b -> r
run algStep params graph = case step algStep params graph of
                                      Left r -> r
                                      Right (newGraph, newParams) -> run algStep newParams newGraph

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

type BFSParams a = ((LNode a -> Bool), [LNode (a,Bool)])

bfsStep :: Eq a => AlgStep (a, Bool) b (BFSParams a) (Maybe (LNode a))
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
        Gr (a, Bool) b ->
        Either (Maybe (LNode a)) (Gr (a, Bool) b, BFSParams a)
bfsStep' (p, []) graph = Left Nothing
bfsStep' (p, (q:qs)) graph | p . removeFlag $ q = Left . Just . removeFlag $ q
                           | otherwise          = Right (newGraph, newParams)
  where --the new graph is the old graph where the labels have been updated accoring to if the nodes have been explored.
        newGraph = nmap f graph
        f (label, True) = (label, True)
        f l@(label, False) = if l == l' then (label, True) else l
        (_, l') = q
        --the new parameters are the same as the old ones, only the queue is appended with unexplored nodes, now marked explored
        newParams = (p, qs ++ unexploredNodes)
        --get all outgoing neighbours of the first node in the queue en check if they have been explored by inspecting their flag
        unexploredNodes = filter (\x -> getFlag x == False) $ listOutNeighbors graph $ fst q


-- runs the bfs algorithm by calling run with the right parameters
bfsRun :: Eq a => (LNode a -> Bool) -> Gr a b -> Maybe (LNode a)
bfsRun p graph = run bfsStep params flaggedGraph
  where params = (p, [addFlag (const True) firstNode])
        flaggedGraph = nmap (\x -> if (Just x == lab graph (fst firstNode)) then (x,True) else (x,False)) graph
        firstNode = head . labNodes $ graph
