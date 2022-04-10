module Main where

import Data.GraphViz
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree

import qualified Data.Map as M

import Algorithms
import Visualization
import Graph
import Gif
import Helper

main :: IO ()
main = do
  createFolderStructure
  putStrLn  "Give the relative path for the gif (default: './resultFolder/gifResults/{n}.gif')"
  gifPath <- getGifPath
  putStrLn  "Give the relative path for the graph you want to use (default: './graphs/default.txt')"
  graph <- getGraph
  putStrLn  "Give the graph size (Only used in SCC) (default: '(250,400)')"
  size <- getSize
  putStrLn  "Give the algorithm you want to visualize (default: 'BFS')"
  line <- getLine
  case line of 
    "" -> uncurry (runAndViz bfsStep bfsViz) (bfsStart graph)
    "BFS" -> uncurry (runAndViz bfsStep bfsViz) (bfsStart graph)
    "DFS" -> uncurry (runAndViz dfsStep dfsViz) (bfsStart graph)
    "SCC" -> uncurry (runAndViz sccStep (sccViz size)) (sccStart graph)
  attemptToCreateGif gifPath

