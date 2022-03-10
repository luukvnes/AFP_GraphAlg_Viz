module Main where

import Graph

main :: IO ()
main = runGraphvizCanvas' Gtk test
