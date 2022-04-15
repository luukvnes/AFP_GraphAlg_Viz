module Algorithms where
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances  #-}


import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree

import Data.Bifunctor
import Data.List
import qualified Data.Map as M

import Graph
import Helper
import GHC.Base (undefined)


-- | The type 'AlgStep' is used to represent a single step of computation of a graph algorithm.
--   It contains a step function, that takes parameters of type 'p' and a graph, and produces
--   either a new graph and set of parameters with which it can continue the computation
--   or a result of type 'r', signaling the algorithm has terminated.
--   'AlgStep' is parameterized by
--   a, the type of the label of the node
--   b, the type of the label of the edge
--   p, the paramters used in the algorithm
--   r, the result type, eg a Path (from start to end node) for bfs

newtype AlgStep a b p r = Step {step :: p -> Gr a b -> Either r (Gr a b, p)}


-- | 'fmap' creates a new step function that alters only the result using f
--   If there is no result f does nothing.
instance Functor (AlgStep a b p) where
  fmap f (Step alg) = Step $ \params graph -> first f $ alg params graph


-- | 'run' executes an algorithm 'step' until it reaches a result, which it then returns.
run :: AlgStep a b p r -> p -> Gr a b -> r
run algStep params graph = case step algStep params graph of
                                      Left  r                     -> r
                                      Right (newGraph, newParams) -> run algStep newParams newGraph
