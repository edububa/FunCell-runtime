{-| This module contains the data types and functions to deal with
    dependencies inside a @SpreadSheet@. -}
module Lib.Dependency where

-- external imports
import Algebra.Graph.AdjacencyMap
import Algebra.Graph.AdjacencyMap.Algorithm
-- internal imports
import Data.Cell
import Data.Dependency
import Data.Set (toList)

{-| 'addDependency' introduces a new dependency to the graph. -}
addDependency :: Index -> Index -> Dependencies -> Dependencies
addDependency from to = overlay $ edge from to

{-| 'removeDependency' deletes the edge representing a dependency in the
  graph. -}
removeDependency :: Index -> Index -> Dependencies -> Dependencies
removeDependency = removeEdge

{-| 'addDependencies' introduces the dependencies between the first
  index and a list of indices -}
addDependencies :: Index -> [Index] -> Dependencies -> Dependencies
addDependencies from tos ds = foldr (addDependency from) ds tos

{-| 'getOrder' -}
getOrder :: Dependencies -> Maybe [Index]
getOrder = topSort

{-| 'resetDependency' deletes the vertex of a given index in the graph,
  this way resets all its dependencies. -}
resetDependency :: Index -> Dependencies -> Dependencies
resetDependency = removeVertex

{-| 'getDependencies' returns a list with the dependencies reachable
  from a given index. -}
getDependencies :: Index -> Dependencies -> [Index]
getDependencies = reachable

{-| 'getDependents' returns a list with the dependent indices of a
  given index. -}
getDependents :: Index -> Dependencies -> [Index]
getDependents from = toList <$> preSet from

{-| 'circularDependencies' is a predicate that checks if a given graph
  has circular dependencies -}
circularDependencies :: Dependencies -> Bool
circularDependencies = not . isAcyclic

{-| 'updateDependency' returns an updated dependency graph, first
  removes the edges of then Index that will be updated, after that the
  new edges are added. -}
updateDependency :: Index -> [Index] -> Dependencies -> Dependencies
updateDependency from tos ds = addDependencies from tos .
                               foldr (removeDependency from) ds .
                               toList . postSet from $ ds
