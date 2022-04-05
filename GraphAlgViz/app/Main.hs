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
  putStrLn  "Give the algorithm you want to visualize (default: 'BFS')"
  (algorithm, visualizer) <- getAlgorithm
  let firstNode = head . labNodes $ graph
  let flaggedGraph = nmap (\x -> if (Just x == lab graph (fst firstNode)) then (x,Queued) else (x,Unexplored)) graph
  let p n@(i,l) = l == "7"
  let params = (p, [addFlag (const Queued) firstNode])
  runAndViz algorithm visualizer params flaggedGraph
  attemptToCreateGif gifPath

-- Is not in helper.hs to avoid circular imports
getAlgorithm = do
    returnAlg <$> getLine

returnAlg "" = (bfsStep, bfsViz)
returnAlg "BFS" = (bfsStep, bfsViz)
returnAlg "DFS" = (dfsStep, bfsViz)
returnAlg _ = error "Algorithm not found"

mainGif :: IO ()
mainGif = do
  let firstNode = head . labNodes $ graph
  let flaggedGraph = nmap (\x -> if (Just x == lab graph (fst firstNode)) then (x,Queued) else (x,Unexplored)) graph
  let p n@(i,l) = l == 7
  let params = (p, [addFlag (const Queued) firstNode])
  createFolderStructure
  runAndViz bfsStep bfsViz params flaggedGraph
  gifPath <- getGifPath
  attemptToCreateGif gifPath

-- graph = fromEdgeList [(1, 2, "A"),
--   (2, 1, "B"),
--   (2, 3, "C"),
--   (4, 3, "D"),
--   (1, 4, "E"),
--   (4, 5, "F"),
--   (5, 6, "G"),
--   (6, 3, "H"),
--   (6, 7, "I"),
--   (7, 8, "J"),
--   (3, 9, "J1"),
--   (4, 9, "J2"),
--   (10, 11, "J3"),
--   (11, 12, "J4"),
--   (11, 13, "J5"),
--   (1, 13, "J6"),
--   (3, 13, "J7")]

graph = fromEdgeList [(0,2,""),
                      (0,3,""),
                      (2,1,""),
                      (1,0,""),
                      (3,4,"")]
mainConsole= do
  let firstNode = head . labNodes $ graph
  let flaggedGraph = nmap (\x -> if (Just x == lab graph (fst firstNode)) then (x,Queued,-1) else (x,Unexplored,-1)) graph
  let params = (1, 0, [], [addFlagSCC (const Queued) firstNode], addFlagSCC (const Queued) firstNode)
  runAndViz sccStep sccViz params flaggedGraph
  attemptToCreateGif "resultFolder/gifResults/0004.gif"