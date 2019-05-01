-- | This module contains the data types and functions to deal with
-- dependencies inside a @SpreadSheet@.
module Lib.Dependency
  ( -- * Dependency Graph Functions
    addDependency
  , addDependencies
  , resetDependency
  , removeDependency
  , updateDependency
    -- * Dependency Analysis Functions
  , getDependencies
  , getDependents
  , circularDependencies
  , getOrder
  ) where

-- external imports
import Algebra.Graph.AdjacencyMap
import Algebra.Graph.AdjacencyMap.Algorithm
-- internal imports
import Data.Cell
import Data.Dependency
import Data.Set (toList)

-- | 'addDependency' introduces a new dependency to the graph.
--
-- >>> addDependency (0,0) (1,1) Data.Dependency.empty
-- edge (0,0) (1,1)
-- >>> addDependency (0,0) (0,1) $ addDependency (0,0) (1,1) Data.Dependency.empty
-- edges [((0,0),(0,1)),((0,0),(1,1))]
addDependency :: Index -> Index -> Dependencies -> Dependencies
addDependency from to = overlay $ edge from to

-- | 'addDependencies' introduces dependencies between the first index
-- and a list of indices
--
-- >>> let x = addDependency (0,0) (0,1) $ addDependency (0,0) (1,1) Data.Dependency.empty
-- >>> let y = addDependencies (0,0) [(0,1), (1,1)] Data.Dependency.empty
-- >>> x
-- edges [((0,0),(0,1)),((0,0),(1,1))]
-- >>> y
-- edges [((0,0),(0,1)),((0,0),(1,1))]
-- >>> x == y
-- True op
addDependencies :: Index -> [Index] -> Dependencies -> Dependencies
addDependencies from tos ds = foldr (addDependency from) ds tos

-- | 'resetDependency' deletes the vertex of a given index in the
-- graph, this way resets all its dependencies.
--
-- >>> let x = addDependencies (0,0) [(0,1), (1,1)] Data.Dependency.empty
-- >>> resetDependency (0,0) x
-- vertices [(0,1),(1,1)]
resetDependency :: Index -> Dependencies -> Dependencies
resetDependency = removeVertex

-- | 'removeDependency' deletes the edge representing a dependency in
-- the graph.
--
-- >>> let x = addDependencies (0,0) [(0,1), (1,1)] Data.Dependency.empty
-- >>> removeDependency (0,0) (0,1) x
-- overlay (vertex (0,1)) (edge (0,0) (1,1))
removeDependency :: Index -> Index -> Dependencies -> Dependencies
removeDependency = removeEdge

-- | 'updateDependency' returns an updated dependency graph, first
-- removes the edges of then Index that will be updated, after that
-- the new edges are added.
--
-- >>> let x = addDependencies (0,0) [(0,1), (1,1)] Data.Dependency.empty
-- >>> updateDependency (0,0) [(1,2), (3,3)] x
-- overlay (vertices [(0,1),(1,1)]) (edges [((0,0),(1,2)),((0,0),(3,3))])
updateDependency :: Index -> [Index] -> Dependencies -> Dependencies
updateDependency from tos ds = addDependencies from tos .
                               foldr (removeDependency from) ds .
                               toList . postSet from $ ds

-- | 'getOrder'
getOrder :: Dependencies -> Maybe [Index]
getOrder = topSort

-- | 'getDependencies' returns a list with the dependencies reachable
-- from a given index.
getDependencies :: Index -> Dependencies -> [Index]
getDependencies = reachable

-- | 'getDependents' returns a list with the dependent indices of a
-- given index.
--
-- >>> let x = addDependencies (0,0) [(0,1), (1,1)] Data.Dependency.empty
-- >>> getDependencies (0,0) x
-- [(0,0),(0,1),(1,1)]
getDependents :: Index -> Dependencies -> [Index]
getDependents from = toList <$> preSet from

-- | 'circularDependencies' is a predicate that checks if a given
-- graph has circular dependencies.
--
-- let x = addDependencies (0,0) [(0,1), (1,1)] Data.Dependency.empty
-- >>> x
-- edges [((0,0),(0,1)),((0,0),(1,1))]
-- >>> circularDependencies x
-- False
-- >>> circularDependencies $ addDependency (0,0) (0,0) x
-- True
circularDependencies :: Dependencies -> Bool
circularDependencies = not . isAcyclic
