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
  putStrLn  "Give the algorithm you want to visualize from the following list (default: 'BFS')"
  putStrLn . show $ tail (fst algorithms) ++ snd algorithms
  line <- getLine
  case (line `elem` fst algorithms) of
    True -> algWithTarget gifPath graph size line
    False -> algWithoutTarget gifPath graph size line

algorithms  = (["", "BFS", "DFS", "Dijkstra"],["SCC"])

algWithTarget :: String -> Gr String String -> (Double, Double) -> String -> IO ()
algWithTarget gifPath graph _ line = do
  putStrLn "Give the target node? Please supply a label from the following lists:"
  let nodeLabels = show . map snd . listNodes $ graph
  putStrLn nodeLabels
  label <- getLine
  case line of
    ""    -> uncurry (runAndViz bfsStep bfsViz) (bfsStart label graph)
    "BFS" -> uncurry (runAndViz bfsStep bfsViz) (bfsStart label graph)
    "DFS" -> uncurry (runAndViz dfsStep dfsViz) (bfsStart label graph)
    "Dijkstra" -> uncurry (runAndViz dijkStep dijkViz) (dijkStart label graph)
    _     -> error "Could not recognize algorithm"
  attemptToCreateGif gifPath

algWithoutTarget :: String -> Gr String String -> (Double, Double) -> String -> IO ()
algWithoutTarget gifPath graph size line = do
  case line of
    "SCC" -> uncurry (runAndViz sccStep (sccViz size)) (sccStart graph)
    _     -> error "Could not recognize algorithm"
  attemptToCreateGif gifPath
