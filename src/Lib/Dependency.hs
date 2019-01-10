{-| This module contains the data types and functions to deal with
    dependencies inside a @SpreadSheet@. -}
module Lib.Dependency where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Cell (Index)
import Data.Set as Set (Set)
import qualified Data.Set as Set

type Dependencies = Map Index (Set Index)

empty :: Dependencies
empty = Map.empty

addDependency :: Index -> Index -> Dependencies -> Dependencies
addDependency from to ds = Map.insert from set ds
  where set = maybe (Set.singleton to) (Set.insert to) (Map.lookup from ds)

deleteDependency :: Index -> Index -> Dependencies -> Dependencies
deleteDependency from to ds = Map.insert from set ds
  where set = maybe Set.empty (Set.delete to) (Map.lookup from ds)

getDependencies :: Index -> Dependencies -> [Index]
getDependencies from = maybe [] Set.elems . (Map.lookup from)

addDependencies :: Index -> Set Index -> Dependencies -> Dependencies
addDependencies from tos ds = Map.insert from set ds
  where set = maybe tos (Set.union tos) (Map.lookup from ds)

addDependencies' :: Foldable f => Index -> f Index -> Dependencies -> Dependencies
addDependencies' to froms ds = foldr f ds froms
  where f x acc = addDependency x to acc
