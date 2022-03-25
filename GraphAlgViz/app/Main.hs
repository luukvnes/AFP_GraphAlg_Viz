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
  str <- readFile "./graphs/example1.txt"
  let graph = parseGraph str
  prettyPrint graph
  {-
  let firstNode = head . labNodes $ graph
  let flaggedGraph = nmap (\x -> if (Just x == lab graph (fst firstNode)) then (x,True) else (x,False)) graph
  let p n@(i,l) = l == "Cat"
  let params = (p, [addFlag (const True) firstNode])
  runAndPrint bfsStep bfsViz params flaggedGraph
  -}
