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


--fmtEdge :: (n, n, el) -> Attributes


--fillColor :: NamedColor nc => nc -> Attribute

graph = fromAdjList [("Lukas","Luuk",'A'), ("Lukas", "Max", 'B')]

test :: DotGraph Node
test = graphToDot params graph


{-
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

bfs :: (LNode a -> Bool) -> Gr a b -> Maybe (LNode a)
bfs p graph = bfs' p markedGraph [explored firstNode]
  where markedGraph = first (\x -> (x,False)) graph --created a new graph which has where every node has a marker, initiaited to False
        firstNode = head.labNodes $ markedGraph
        explored (n, (x,_)) = (n, (x, True))


bfs' :: (LNode a -> Bool) -> Gr (a, Bool) b -> [LNode (a,Bool)] -> Maybe (LNode a)
-- bfs' :: predicate -> graph -> queue -> found node (if it exists)
bfs' p graph [] = Nothing
bfs' p graph (q:qs) = undefined
  --  if p q then Just ((\(n, (x,_)) -> (n,x)) q)
  --                     else undefined
