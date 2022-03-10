module Main where

import Graph
import Data.GraphViz.Commands

main :: IO ()
main = runGraphvizCanvas' test Xlib
