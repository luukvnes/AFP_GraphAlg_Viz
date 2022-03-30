module Main where

import Data.GraphViz
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree

import Algorithms
import Visualization
import Graph
import Gif
import Helper


main :: IO ()
main = do
  let firstNode = head . labNodes $ graph
  let flaggedGraph = nmap (\x -> if (Just x == lab graph (fst firstNode)) then (x,Queued) else (x,Unexplored)) graph
  let p n@(i,l) = l == 7
  let params = (p, [addFlag (const Queued) firstNode])
  runAndPrettyPrint bfsStep params flaggedGraph
  --runGraphvizCanvas Dot (bfsViz' flaggedGraph) Xlib

mainGif :: IO ()
mainGif = do
  let firstNode = head . labNodes $ graph
  let flaggedGraph = nmap (\x -> if (Just x == lab graph (fst firstNode)) then (x,Queued) else (x,Unexplored)) graph
  let p n@(i,l) = l == 7
  let params = (p, [addFlag (const Queued) firstNode])
  createFolderStructure
  runAndPrint bfsStep bfsViz params flaggedGraph
  attemptToCreateGif

graph = fromEdgeList [(1,2,""),
                      (2,1,""),
                      (2,3,""),
                      (4,3,""),
                      (1,4,""),
                      (4,5,""),
                      (5,6,""),
                      (6,3,""),
                      (6,7,""),
                      (7,1,"")]
