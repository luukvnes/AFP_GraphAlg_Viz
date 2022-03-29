module Visualization where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree

import Data.GraphViz
import Data.GraphViz.Attributes.Colors
import Data.GraphViz.Attributes.Complete

import System.Directory

import Algorithms
import Helper

newtype AlgorithmViz a b = Viz (Gr a b -> DotGraph Node)

-- current bug with creating the inner directory, for now run twice.
visualize :: AlgorithmViz a b -> Gr a b -> IO ()
visualize (Viz alg) graph = do
    exists <- doesDirectoryExist "resultFolder"
    if exists then pure () else createDirectory "resultFolder"
    dirs <- listDirectory "resultFolder"
    if null dirs then createDirectory "resultFolder/results1"  else pure ()--todo create new directory every run
    let dir = last dirs
    files <- listDirectory ("resultFolder./" ++ dir)
    let lastFile = if null files then "0.bmp" else head files
    let newFileName = "resultFolder./" ++ dir ++ "./" ++ (incrementFileName lastFile)
    str <- runGraphviz (alg graph) Bmp newFileName
    putStrLn str

--same as run, but prints the graph to the terminal at every step
runAndPrint :: (Show r, Show a, Show b) => AlgStep a b p r -> AlgorithmViz a b -> p -> Gr a b -> IO ()
runAndPrint algStep algViz params graph = case step algStep params graph of
                                        Left r -> print "Final graph" >> visualize algViz graph
                                        Right (newGraph, newParams) -> visualize algViz graph >> print "--------------------------" >> runAndPrint algStep algViz newParams newGraph

runAndPrettyPrint :: (Show r, Show a, Show b) => AlgStep a b p r -> p -> Gr a b -> IO ()
runAndPrettyPrint algStep params graph = case step algStep params graph of
                                              Left r -> print "Final graph" >> prettyPrint graph >> (print ("Result is: " ++ show r))
                                              Right (newGraph, newParams) -> prettyPrint graph >> print "--------------------------" >> runAndPrettyPrint algStep newParams newGraph

-- bfsStep :: Eq a => AlgStep (a, Bool) b (BFSParams a) (Maybe (LNode a))
bfsViz :: Eq a => Ord b => AlgorithmViz (a, Bool) b
bfsViz = Viz bfsViz'

bfsViz' :: Ord b => Gr (a, Bool) b -> DotGraph Node
bfsViz' graph = setDirectedness graphToDot params graph
  where
    params = blankParams { globalAttributes = []
                         , clusterBy        = clustBy
                         , clusterID        = Num . Int
                         , isDotCluster     = const True
                         , fmtCluster       = const []
                         , fmtNode          = fmtNode
                         , fmtEdge          = const []
                         }
    clustBy (n,l) = C 1 $ N (n,l)
    fmtNode (a, (_, True)) = [Color [WC (X11Color Aquamarine4) Nothing] ] --, color (X11Color Aquamarine4)
    fmtNode (a, (_, False)) = [Color [WC (X11Color Red) Nothing] ] --, color (X11Color Aquamarine4)
