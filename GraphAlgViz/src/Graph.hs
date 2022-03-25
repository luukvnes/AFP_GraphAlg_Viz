module Graph where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Data.List
import Data.List.Split


type LEdge b = (Node, Node, b)

type Label = String
type EdgeList a b = [(a, a, b)] --list of edges, fromatted as (fromNodeLabel, toNodeLabel, edgeLabel)

-- Want function that takes an edge list and converts it into a graph.

fromAdjList :: (Ord a) => EdgeList a b -> Gr a b
fromAdjList adjList = mkGraph nodes edges
  where nodes = zip [1..] $ sortedNodeLabels
        edges = foldMap g [ (i,j,lab) | (from, to, lab) <- adjList, let i = f from, let j = f to]
        f lab = fst <$> find (\x -> snd x == lab) nodes
        g (Just i,Just j,lab) = [(i,j,lab)]
        g _                   = []
        sortedNodeLabels = map head . group . sort . concat $ [ [from, to] | (from,to,_) <- adjList]

--Takes a string formatted where each line corresponds to an edge and returns graph
--Every line is formatted as "from, to, label"
parseGraph :: String -> Gr String String
parseGraph = fromAdjList . map (\(from:to:edgeLabel:_) -> (from, to, edgeLabel)) . map (splitOn ", ") . lines


--gets all labeled neighbours of a node
listNeighbors :: Graph gr => gr a b -> Node -> [LNode a]
listNeighbors gr n = foldMap f [ (i, lab gr i) | i <- ns]
    where ns = neighbors gr n
          f (i, Just l) = [(i, l)]
          f (_, Nothing) = []

listOutNeighbors :: Graph gr => gr a b -> Node -> [LNode a]
listOutNeighbors gr n = foldMap f [ (i, lab gr i) | i <- ns]
    where ns = suc gr n
          f (i, Just l) = [(i, l)]
          f (_, Nothing) = []
