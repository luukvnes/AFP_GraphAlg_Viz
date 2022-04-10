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
import GHC.Word

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



-- --same as run, but prints the graph to the terminal at every step
-- runAndViz :: (Show r, Show a, Show b) => AlgStep a b p r -> AlgorithmViz a b -> p -> Gr a b -> IO ()
-- runAndViz algStep algViz params graph = case step algStep params graph of
--                                         Left r -> print "Final graph" >>  print graph >> (print ("Result is: " ++ show r))
--                                         Right (newGraph, newParams) -> print graph >> runAndViz algStep algViz newParams newGraph

runAndViz :: (Show r, Show a, Show b) => AlgStep a b p r -> AlgorithmViz a b -> p -> Gr a b -> IO ()
runAndViz algStep algViz params graph = case step algStep params graph of
                                        Left r -> print "Final graph" >>  visualize algViz graph >> (print ("Result is: " ++ show r))
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
    fmtNode (_, (l, Unexplored)) = [Color [WC (X11Color Blue) Nothing], label l ]
    fmtNode (_, (l, Queued)) = [Color [WC (X11Color Red) Nothing], label l ]
    fmtNode (_, (l, Explored)) = [Color [WC (X11Color Green) Nothing], label l ]

    label :: Show a => a -> Attribute
    label = Label . StrLabel . pack . filter (/='"') . show

dfsViz :: (Eq a, Show a, Ord b) => AlgorithmViz (a, Flag) b
dfsViz = bfsViz

sccViz :: (Eq a, Show a, Ord b) => (Double, Double) -> AlgorithmViz (a, Flag, Int) b
sccViz size = Viz (sccViz' size)

sccViz' :: (Show a, Ord b) => (Double, Double) -> Gr (a, Flag, Int) b -> DotGraph Node
sccViz' (width, height) graph = setDirectedness graphToDot params graph
  where
    params = blankParams { globalAttributes = [GraphAttrs [ViewPort (VP width height 1 Nothing)]]
                         , clusterBy        = clustBy
                         , clusterID        = Num . Int
                         , isDotCluster     = const True
                         , fmtCluster       = const []
                         , fmtNode          = fmtNode
                         , fmtEdge          = const []
                         }
    clustBy (n,l) = C 1 $ N (n,l)
    fmtNode (_, (l, Unexplored, _)) = [Color [WC (X11Color Blue) Nothing], label l ]
    fmtNode (_, (l, Queued, _)) = [Color [WC (X11Color Red) Nothing], label l ]
    -- if in step 1
    --    then we give it a green colour depending on when it was added to the stack
    -- in step 3, 
    --    then we give it a random colour from a list of fairly random colours.
    fmtNode (_, (l, Explored, s)) | s > sizeOfStack = [Color [WC (colorsForStack !! ((s-sizeOfStack) `mod` 20)) Nothing], label l ]
                                  | otherwise = [Color [WC (rgbFromStackID s) Nothing], label l ]


    -- 20 colors to use to colours the stack
    colorsForStack = [X11Color Brown, X11Color Aquamarine1, X11Color DarkOliveGreen, X11Color DarkOrange, X11Color DeepPink, X11Color Gold, X11Color IndianRed, X11Color OrangeRed, X11Color LawnGreen, X11Color Magenta, X11Color MediumPurple, X11Color Moccasin, X11Color OliveDrab, X11Color Purple, X11Color Salmon, X11Color Sienna, X11Color Tomato, X11Color Yellow, X11Color Violet, X11Color Turquoise, X11Color Thistle]

    -- gets a green rgb value with its brightness depending on when a node was put in the stack
    rgbFromStackID sid = RGB 0 (fromIntegral  ((((sid+1) * 200) `div` (sizeOfStack+1))+55)) 0

    -- the size of sccStack
    sizeOfStack = sizeOfStack' 1
    sizeOfStack' n = if stackIDAssigned n (labNodes graph) then sizeOfStack' (n+1) else n-1

    -- wether the sid given as paramater is in the graph
    stackIDAssigned _ [] = False
    stackIDAssigned sid1 ((_,(_,_,sid2)):xs) =  sid1 == sid2 || stackIDAssigned sid1 xs

    label :: Show a => a -> Attribute
    label = Label . StrLabel . pack . filter (/='"') . show