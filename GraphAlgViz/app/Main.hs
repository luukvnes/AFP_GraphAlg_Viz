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

  putStrLn  "Give the relative path for the gif (default: './resultFolder/gifResults/{n}.gif')"
  gifPath <- getGifPath
  putStrLn  "Give the relative path for the graph you want to use (default: './graphs/default.txt')"
  graph <- getGraph
  putStrLn  "Give the algorithm you want to visualize (default: 'BFS')"
  (algorithm, visualizer) <- getAlgorithm
  let firstNode = head . labNodes $ graph
  let flaggedGraph = nmap (\x -> if (Just x == lab graph (fst firstNode)) then (x,Queued) else (x,Unexplored)) graph
  let p n@(i,l) = l == "HJ"
  let params = (p, [addFlag (const Queued) firstNode])
  createFolderStructure
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
