module Main where

import Data.GraphViz
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree

import GraphAlgorithms
import Graph


main :: IO ()
main = runAndPrint bfsStep params flaggedGraph
  where params = (p, [addFlag (const True) firstNode])
        p n@(i,l) = l == "Maasdfasdf"
        flaggedGraph = nmap (\x -> if (Just x == lab graph (fst firstNode)) then (x,True) else (x,False)) graph
        firstNode = head . labNodes $ graph



graph = fromAdjList [("Lukas","Luuk",'A'),
                     ("Lukas", "Max", 'B'),
                     ("Luuk", "Max" , 'C'),
                     ("Luuk", "Taco" , 'D'),
                     ("Taco", "Mark" , 'E'),
                     ("Mark", "Lukas" , 'F')
                    ]
