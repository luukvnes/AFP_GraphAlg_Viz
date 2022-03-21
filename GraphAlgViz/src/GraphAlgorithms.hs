module GraphAlgorithms where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Graph

newtype AlgStep a b p r = Step (p -> Gr a b -> Either r (Gr a b, p))
newtype AlgorithmViz a b p = Viz (p -> Gr a b -> (Gr a b, p))
{- Parameterized by
    a, the type of the label of the node
    b, the type of the label of the edge
    p, the paramters used in the algorithm
    r, the result type, eg a Path (from start to end node) for bfs
-}

-- executes a stingle step of the algorithm defined by AlgStep
step :: AlgStep a b p r -> p -> Gr a b -> Either r (Gr a b, p)
step (Step alg) params graph = alg params graph

--executes step until it reaches a result.
run :: AlgStep a b p r -> p -> Gr a b -> r
run algStep params graph = case step algStep params graph of
                                      Left r -> r
                                      Right (newGraph, newParams) -> run algStep newParams newGraph


runAndPrint :: (Show r, Show a, Show b) => AlgStep a b p r -> p -> Gr a b -> IO ()
runAndPrint algStep params graph = case step algStep params graph of
                                        Left r -> print "Final graph" >> prettyPrint graph >> print ("Result: " ++ show r)
                                        Right (newGraph, newParams) -> prettyPrint graph >> print "--------------------------" >> runAndPrint algStep newParams newGraph

--takes a way of visualizing a graph and a graph and visualizes it.
visualize :: AlgorithmViz a b p -> Gr a b -> IO ()
visualize = undefined


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

bfsRun :: Eq a => (LNode a -> Bool) -> Gr a b -> Maybe (LNode a)
bfsRun p graph = run bfsStep params flaggedGraph
  where params = (p, [addFlag (const True) firstNode])
        flaggedGraph = nmap (\x -> if (Just x == lab graph (fst firstNode)) then (x,True) else (x,False)) graph
        firstNode = head . labNodes $ graph

type BFSParams a = ((LNode a -> Bool), [LNode (a,Bool)])

bfsStep :: Eq a => AlgStep (a, Bool) b (BFSParams a) (Maybe (LNode a))
-- bfs has params
-- (LNode a -> Bool), a function that checks if the found node is the one were looking for
-- [LNode (a,Bool)], a queue
-- returns the node if it finds one.
bfsStep = Step bfsStep'

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

--add a boolean flag to the label type using a tuple and a function from nodes to booleans
addFlag :: (LNode a -> Bool) -> LNode a -> LNode (a,Bool)
addFlag p n@(node,label) = (node, (label,p n))

setFlag :: (LNode (a,Bool) -> Bool) -> LNode (a,Bool) -> LNode (a,Bool)
setFlag p n@(node,(label, _)) = (node, (label,p n))

--extract a boolean flag from a node
getFlag :: LNode (a,Bool) -> Bool
getFlag (_,(_,b)) = b

--remove a flag from a node.
removeFlag :: LNode (a,Bool) -> LNode a
removeFlag (n,(l,_)) = (n,l)






-- iterator :: IO ()
-- iterator algorithm graph c = let result = step algorithm in
--     case result of
--         Left r -> return ()
--         Right Graph -> visualize graph >> iterator algorithm newGraph c

-- run :: IO ()
-- run alg graph c = do
--     iterator ....
--     gif <- makeGif filepath .. :: IO ()
--     render gif

-- takewhile p

-- map toPicture


-- instance Monad Algorithm where
--     alg >>= f = saveGraphAsPicture graph
--                 f graph


-- g2 = insEdges [(1,2, ())] (insNodes [(2, 2), (3, 3), (4, 4)] graph)
