module Algorithms.Dijkstra where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree

import Algorithms

import Data.Bifunctor
import Data.List
import qualified Data.Map as M

import Graph
import Helper
import GHC.Base (undefined)


------------------Dijkstra--------------------------------------------


-- | 'DijkParams' conatins the parameters needed to run Dijkstra's shortest path algorithm
--   It consists of a queue and and a distance list, which stores
--   all the current information about the distance to the source node and which
--   node is the first node on the shortest path

type DijkParams a = (Q a, DistList a)

--distance list is a dictionary storing a node, together wits its distance to the source and the preveous node on the route
type DistList a = M.Map (LNode (a, Flag)) (Int, LNode (a, Flag))
type Q a = [LNode (a, Flag)]


-- | 'dijkStep' is the single step implementation of Dijkstra's shortest path algorithm
--   It returns a list of containing the distances of all nodes to a source node,
--   as well as which node is first on the shortest path to the source node.
--   It requires the node labels to have a flag of type 'Flag'
dijkStep :: Eq a => AlgStep (a, Flag) b (DijkParams a) [(LNode a, Int, LNode a)]
dijkStep = fmap f $ Step dijkStep'
  --map f to format the output a little bit
  where f = map (\(node, (dist, prev)) -> (removeFlag node, dist, removeFlag prev)) . M.toAscList

dijkStep' :: Eq a => DijkParams a -> Gr (a, Flag) b -> Either (DistList a) (Gr (a, Flag) b, DijkParams a)
--if the queue is empty, return the distance and preveous lists.
dijkStep' ([], dists) _ = Left dists
dijkStep' (q, dists) graph = Right (newGraph, newParams)
  where --find the node with minimal distance in the queue
        minDistInfo                       = foldr1 minTuple . filter ((flip elem) q . fst) . M.toList $ dists
        minTuple x@(_,(d,_)) y@(_,(d',_)) = if d < d' then x else y
        --extract the actual node and the distance
        minDistNode = fst minDistInfo
        minDist     = fst . snd $ minDistInfo
        --get all the neighbours in the queue
        neighbors = filter ((flip elem) q) $ listOutNeighbors graph $ fst minDistNode
        --check if there is a shorter path using f and update accordingly
        --use of `listNeighbors` instead of `listOutNeighbours` makes this algorithm undirected.
        newDists  = M.mapWithKey f dists
        f n (i,x) = if p n (i,x) then (1 + minDist, minDistNode) else (i,x)
        p n (i,_) = n `elem` neighbors && (i > (1 + minDist))
        newParams = (delete minDistNode q, newDists)
        -- update flags. If the node is queued then mark it explored. Mark the next node as queued and keep all other labels the same.
        newGraph = nmap updateFlag graph
        
        updateFlag l | l == snd minDistNode = (fst l, Queued) --set the node to queued if it is the current node
        updateFlag l | snd l == Queued      = (fst l, Explored) --if the node was preveously queued, set it to explored
        updateFlag l | otherwise            = (fst l, snd l) --otherwise, keep the same flag.



-- | 'dijkRun' runs the Dijkstra's algorithm by calling run with the initial parameters
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

-- | creates inital parameters for 'dijkStep' from a graph 'Gr'
--   and a source node with a label of type 'a'
dijkStart :: (Eq a, Ord a) => a -> Gr a b -> (DijkParams a, Gr (a, Flag) b)
dijkStart label graph = (params, flaggedGraph)
  where params             = (q, dists)
        -- initial queue consists of all nodes
        q                  = listNodes flaggedGraph
        -- distances are unexplored, except for the source node.
        dists              = M.mapWithKey f $ M.fromList $ zip q (repeat (maxBound, (i,(l,Unexplored))))
        f (i',(l',Queued)) _ = (0, (i',(l',Unexplored)))
        f _              v = v
        flaggedGraph       = nmap (\x -> if x == label then (x,Queued) else (x,Unexplored)) graph
        source@(i,l) = case (filter (\x -> label == snd x) $ listNodes graph) of
                        [] -> error "Could not find source node when initializing Dijkstra's algorithm"
                        (x:_) -> x



{-
Psuedocode of the algorithm implemented
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
