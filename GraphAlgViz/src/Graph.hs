module Graph where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Data.List
import Data.List.Split


type Label = String
-- | 'EdgeList' is an intermediary representation of a graph using an edge list.
--   formatted as (fromNodeLabel, toNodeLabel, edgeLabel)
type EdgeList a b = [(a, a, b)]


-- | 'fromEdgeList' takes an 'EdgeList' and converts it into a graph of type 'Gr'
fromEdgeList :: (Ord a) => EdgeList a b -> Gr a b
fromEdgeList adjList = mkGraph nodes edges
  where nodes = zip [1..] $ sortedNodeLabels
        edges = foldMap g [ (i,j,lab) | (from, to, lab) <- adjList, let i = f from, let j = f to]

        f lab = fst <$> find (\x -> snd x == lab) nodes

        g (Just i,Just j,lab) = [(i,j,lab)]
        g _                   = []

        sortedNodeLabels = map head . group . sort . concat $ [ [from, to] | (from,to,_) <- adjList]

-- | 'parseGraph' takes a string where each line corresponds to an edge and returns graph.
--   Every line is needs to be formatted as as "fromNode, toNode, EdgeLabel"
parseGraph :: String -> Gr String String
parseGraph = fromEdgeList . map (\(from:to:edgeLabel:_) -> (from, to, edgeLabel)) . map (splitOn ", ") . lines


-- | 'listNeighbors' lists all labeled neighbours of a node
listNeighbors :: Graph gr => gr a b -> Node -> [LNode a]
listNeighbors gr n = foldMap f [ (i, lab gr i) | i <- ns]
    where ns = neighbors gr n

          f (i, Just l)  = [(i, l)]
          f (_, Nothing) = []

-- | 'listOutNeighbors' lists all labeled neighbours connected by outgoing edges of node
listOutNeighbors :: Graph gr => gr a b -> Node -> [LNode a]
listOutNeighbors gr n = foldMap f [ (i, lab gr i) | i <- ns]
    where ns = suc gr n

          f (i, Just l)  = [(i, l)]
          f (_, Nothing) = []

-- | 'listOutNeighbors' lists all labeled neighbours connected by incoming edges of node
listInNeighbors :: Graph gr => gr a b -> Node -> [LNode a]
listInNeighbors gr n = foldMap f [ (i, lab gr i) | i <- ns]
    where ns = pre gr n
    
          f (i, Just l)  = [(i, l)]
          f (_, Nothing) = []

-- | 'listNodes' lists all nodes of a graph with their labels.
listNodes :: Gr a b -> [LNode a]
listNodes graph = ufold f [] graph
  where f (_, i, l, _) xs = (i,l) : xs
