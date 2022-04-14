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

newtype AlgStep a b p r = Step {step :: p -> Gr a b -> Either r (Gr a b, p)}
{- Parameterized by
    a, the type of the label of the node
    b, the type of the label of the edge
    p, the paramters used in the algorithm
    r, the result type, eg a Path (from start to end node) for bfs
-}

-- Creates a new step function that alters the result using f
-- If there is no result f does nothing.
instance Functor (AlgStep a b p) where
  fmap f (Step alg) = Step $ \params graph -> first f $ alg params graph


--executes step until it reaches a result.
run :: AlgStep a b p r -> p -> Gr a b -> r
run algStep params graph = case step algStep params graph of
                                      Left r -> r
                                      Right (newGraph, newParams) -> run algStep newParams newGraph


instance (Eq a) => Eq ((LNode a -> Bool)) where
        a == b = True
instance (Show a) => Show ((LNode a -> Bool)) where
        show a = "f"

