module Graph where

import Data.GraphViz
import Data.GraphViz.Attributes.Colors
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree

someFunc :: IO ()
someFunc = putStrLn "someFunc"

cl = toWC (RGB 1 2 3)



graph :: Gr String ()
graph = mkGraph [(1,"Luuk")] []

test :: DotGraph Node
test = graphToDot nonClusteredParams graph
