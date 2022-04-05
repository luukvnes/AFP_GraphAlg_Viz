module Visualization where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree

import Data.GraphViz
import Data.GraphViz.Attributes.Colors
import Data.GraphViz.Attributes.Complete

import Control.Monad
import System.Directory
import Data.Text.Lazy (pack)

import Algorithms
import Helper
import Data.List

newtype AlgorithmViz a b = Viz (Gr a b -> DotGraph Node)

-- current bug with creating the inner directory, for now run twice.
visualize :: AlgorithmViz a b -> Gr a b -> IO ()
visualize (Viz alg) graph = do
    dirs <- listDirectory "resultFolder/ImageFolders"
    let dir = head dirs
    files <- listDirectory ("resultFolder/ImageFolders/" ++ dir)
    let lastFile = if null files then "0.bmp" else last (sort files)
    print lastFile
    let newFileName = "resultFolder/ImageFolders/" ++ dir ++ "/" ++ incrementFileName lastFile ".bmp"
    str <- runGraphviz (alg graph) Bmp newFileName
    putStrLn str



--same as run, but prints the graph to the terminal at every step
runAndViz :: (Show r, Show a, Show b) => AlgStep a b p r -> AlgorithmViz a b -> p -> Gr a b -> IO ()
runAndViz algStep algViz params graph = case step algStep params graph of
                                        Left r -> print "Final graph" >> visualize algViz graph
                                        Right (newGraph, newParams) -> visualize algViz graph >> runAndViz algStep algViz newParams newGraph

runAndPrettyPrint :: (Show r, Show a, Show b, Show p) => AlgStep a b p r -> p -> Gr a b -> IO ()
runAndPrettyPrint algStep params graph = case step algStep params graph of
                                              Left r -> print "Final graph" >> prettyPrint graph >> (print ("Result is: " ++ show r))
                                              Right (newGraph, newParams) -> prettyPrint graph >> print newParams >> print "--------------------------" >> runAndPrettyPrint algStep newParams newGraph

-- bfsStep :: Eq a => AlgStep (a, Bool) b (BFSParams a) (Maybe (LNode a))
bfsViz :: (Eq a, Show a, Ord b) => AlgorithmViz (a, Flag) b
bfsViz = Viz bfsViz'

bfsViz' :: (Show a, Ord b) => Gr (a, Flag) b -> DotGraph Node
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
    fmtNode (n, (l, Unexplored)) = [Color [WC (X11Color Blue) Nothing], label l ]
    fmtNode (n, (l, Queued)) = [Color [WC (X11Color Red) Nothing], label l ]
    fmtNode (a, (l, Explored)) = [Color [WC (X11Color Green) Nothing], label l ]

    label :: Show a => a -> Attribute
    label = Label . StrLabel . pack . filter (/='"') . show

dfsViz :: (Eq a, Show a, Ord b) => AlgorithmViz (a, Flag) b
dfsViz = bfsViz

sccViz :: (Eq a, Show a, Ord b) => AlgorithmViz (a, Flag, Int) b
sccViz = Viz sccViz'

sccViz' :: (Show a, Ord b) => Gr (a, Flag, Int) b -> DotGraph Node
sccViz' graph = setDirectedness graphToDot params graph
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
    fmtNode (n, (l, Unexplored, s)) = [Color [WC (X11Color Blue) Nothing], label l ]
    fmtNode (n, (l, Queued, s)) = [Color [WC (X11Color Red) Nothing], label l ]
    fmtNode (a, (l, Explored, 0)) = [Color [WC (X11Color DarkSeaGreen) Nothing], label l ]
    fmtNode (a, (l, Explored, 1)) = [Color [WC (X11Color DarkSeaGreen1) Nothing], label l ]
    fmtNode (a, (l, Explored, 2)) = [Color [WC (X11Color DarkSeaGreen2) Nothing], label l ]
    fmtNode (a, (l, Explored, 3)) = [Color [WC (X11Color DarkSeaGreen3) Nothing], label l ]
    fmtNode (a, (l, Explored, 4)) = [Color [WC (X11Color DarkSeaGreen4) Nothing], label l ]
    fmtNode (a, (l, Explored, 5)) = [Color [WC (X11Color Yellow) Nothing], label l ]
    fmtNode (a, (l, Explored, 6)) = [Color [WC (X11Color Purple) Nothing], label l ]
    fmtNode (a, (l, Explored, 7)) = [Color [WC (X11Color Navy) Nothing], label l ]
    fmtNode (a, (l, Explored, 8)) = [Color [WC (X11Color DarkSeaGreen3) Nothing], label l ]

    label :: Show a => a -> Attribute
    label = Label . StrLabel . pack . filter (/='"') . show