module Graph where

import Data.GraphViz
import Data.GraphViz.Attributes.Colors
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import qualified Data.Set as S
import Data.List
import Data.Bifunctor
import GHC.Base (undefined)

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

params :: GraphvizParams Node nl el () nl
params = Params { isDirected       = True
                 , globalAttributes = []
                 , clusterBy        = N
                 , isDotCluster     = const True
                 , clusterID        = const (Num $ Int 0)
                 , fmtCluster       = const []
                 , fmtNode          = fmtNode' (\(n,_) -> n==1 )
                 , fmtEdge          = const []
                }


--takes a predicate p and only colors the node if p is true
fmtNode' :: (LNode a -> Bool) -> LNode a -> Attributes
fmtNode' p n = if p n then [fillColor Red, style filled] else []
