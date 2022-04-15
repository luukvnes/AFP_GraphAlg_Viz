module Main where

import Data.GraphViz
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree

import qualified Data.Map as M

import Algorithms
import Algorithms.BFS
import Algorithms.DFS
import Algorithms.Dijkstra
import Algorithms.SCC
import Visualization
import Graph
import Gif
import Helper


-- | 'main' runs an algorithm of choice from the list of implemented 'algorithms'
--   on a text representation of any graph,
--   and creates a GIF, visualizing the algorithm
main :: IO ()
main = do
  createFolderStructure
  putStrLn  "Give the relative path for the gif (default: './resultFolder/gifResults/{n}.gif')"
  gifPath <- getGifPath
  putStrLn  "Give the relative path for the graph you want to use (default: './graphs/default.txt')"
  graph   <- getGraph
  putStrLn  "Give the algorithm you want to visualize from the following list (default: 'BFS')"
  print $ tail (fst algorithms) ++ snd algorithms
  line    <- getLine
  case line `elem` fst algorithms of
    True  -> algWithTarget gifPath graph line
    False -> algWithoutTarget gifPath graph line


-- | list of implemented algorithms.
--   The first list contains all algorithms with which need to be supplied with a node,
--   and the second list contains algorithms where this is not needed.
algorithms :: ([String],[String])
algorithms  = (["", "BFS", "DFS", "Dijkstra"],["SCC"])

-- | function
algWithTarget :: String -> Gr String String -> String -> IO ()
algWithTarget gifPath graph line = do
  putStrLn "Give the target node? Please supply a label from the following lists:"
  let nodeLabels = show . map snd . listNodes $ graph
  putStrLn nodeLabels
  label <- getLine
  case line of
    ""         -> uncurry (runAndViz bfsStep bfsViz) (bfsStart label graph)
    "BFS"      -> uncurry (runAndViz bfsStep bfsViz) (bfsStart label graph)
    "DFS"      -> uncurry (runAndViz dfsStep dfsViz) (bfsStart label graph)
    "Dijkstra" -> uncurry (runAndViz dijkStep dijkViz) (dijkStart label graph)
    _          -> error "Could not recognize algorithm"
  attemptToCreateGif gifPath

algWithoutTarget :: String -> Gr String String -> String -> IO ()
algWithoutTarget gifPath graph line = do
  putStrLn  "Give the graph size (Only used in SCC) (default: '(250,400)')"
  size <- getSize
  case line of
    "SCC" -> uncurry (runAndViz sccStep (sccViz size)) (sccStart graph)
    _     -> error "Could not recognize algorithm"
  attemptToCreateGif gifPath
