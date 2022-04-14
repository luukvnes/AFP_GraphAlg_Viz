module Algorithms.SCC where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree

import Algorithms

import Data.Bifunctor
import Data.List
import qualified Data.Map as M

import Graph
import Helper
import GHC.Base (undefined)



------------------SCC--------------------------------------------

{-
1  function SCC(Graph, source):
2
3      DFS (restarting at unexplored vertices) adding vertices to a stack whenever they have no unexplored vertices
4      reverse all edge directions
5      DFS restarting at unexplored vertex top of the stack, every vertex you discover from a single starting vertex belongs to scc
-}

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
