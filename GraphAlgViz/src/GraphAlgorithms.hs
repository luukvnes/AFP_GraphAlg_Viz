module GraphAlgorithms where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree

{- Parameterized by 
    a, the type of the label of the node
    b, the type of the label of the edge
    p, the paramters used in the algorithm
    r, the result type, eg a Path (from start to end node) for bfs
-}
    
newtype AlgorithmStep a b p r = Step (p -> Gr a b -> Either r (Gr a b, p))

step :: AlgorithmStep a b p r -> p -> Gr a b -> Either r (Gr a b, p)
step (Step alg) parameters graph = alg parameters graph



bfs :: AlgorithmStep Int () String String
bfs = undefined

ex = step bfs "dsag" graph

graph :: Gr Int ()
graph = mkGraph [(1, 1)] []

newtype AlgorithmStep a b p = Viz (p -> Gr a b -> (Gr a b, p))
visualize :: AlgorithmViz -> Gr a b -> IO ()

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
