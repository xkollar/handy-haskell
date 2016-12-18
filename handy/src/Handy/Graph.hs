{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module:       $HEADER$
-- Description:  Simple tools for working with graph-like data structures.
--
-- Simple tools for working with graph-like data structures.
module Handy.Graph
    ( Graph(..)
    , bfs
    , dfs
    -- * Some graph-like structures
    , ImplicitGraph(..)
    ) where

import Control.Arrow
import Data.List (foldl')
import qualified Data.Set as Set

import Handy.Container


-- | Ds @graph v l@ is a reprezentation of a graph with
-- vertices of type @v@ and edges labelled by @l@.
class Graph graph where
    neighbours :: graph v l -> v -> [(v, l)]

-- | Various algorithms on graphs in one package.
grKata :: (Ord v, Graph g, Container c (v,a)) => ((v,a) -> c (v,a)) -> (a -> (v,l) -> a) -> a -> g v l -> v -> [a]
grKata sing f u gr pt = go (Set.singleton pt) $ sing (pt, u) where
    go acc c = case get c of
        Just ((v,a),q) -> a : go acc' q' where
            nbrs = neighbours gr v
            acc' = foldr Set.insert acc $ map fst nbrs
            q' = foldl' (flip insert) q . map (fst &&& f a) $ filter (flip Set.notMember acc . fst) nbrs
        Nothing -> []

-- Breadth-first search walk through nodes
--
-- Example: Nodes reachable from given point in order as visited by BFS.
--
-- @
-- bfsOrder :: (Ord v, Graph g) => g v l -> v -> [v]
-- bfsOrder gr p = bfs (const fst) p gr p
-- @
--
-- Example: Paths from start to every reachable node as through BFS.
--
-- @
-- bfsPaths :: (Ord v, Graph g) => g v l -> v -> [[v]]
-- bfsPaths = bfs (flip ((:) . fst)) []
-- @
bfs :: (Ord v, Graph g) => (a -> (v,l) -> a) -> a -> g v l -> v -> [a]
bfs = grKata (singleton :: a -> Queue a)

dfs :: (Ord v, Graph g) => (a -> (v,l) -> a) -> a -> g v l -> v -> [a]
dfs = grKata (singleton :: a -> [a])

-- | Implicit graph (given vertex returns neigbours and labels of edges that
-- led to those neighbours).
newtype ImplicitGraph v l = ImplicitGraph (v -> [(v, l)])

instance Graph ImplicitGraph where
    neighbours (ImplicitGraph f) = f
