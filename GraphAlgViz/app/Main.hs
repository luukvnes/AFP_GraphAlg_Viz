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
main = runAndPrint bfsStep bfsViz params flaggedGraph
  where params = (p, [addFlag (const True) firstNode])
        p n@(i,l) = l == "Maasdfasdf"
        flaggedGraph = nmap (\x -> if (Just x == lab graph (fst firstNode)) then (x,True) else (x,False)) graph
        firstNode = head . labNodes $ graph

createGif = attemptToCreateGif

graph = fromAdjList [("Lukas","Luuk",'A'),
                     ("Lukas", "Max", 'B'),
                     ("Luuk", "Max" , 'C'),
                     ("Luuk", "Taco" , 'D'),
                     ("Taco", "Mark" , 'E'),
                     ("Mark", "Lukas" , 'F')
                    ]
