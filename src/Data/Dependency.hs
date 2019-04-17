{-| This module contains the definition of the Dependency data type. -}
module Data.Dependency where

-- external imports
import Algebra.Graph.AdjacencyMap
-- internal imports
import Data.Cell

{-| 'Dependencies' will be stored in a directed graph. -}
type Dependencies = AdjacencyMap Index

{-| The 'empty' value of an empty dependency graph. -}
empty :: Dependencies
empty = Algebra.Graph.AdjacencyMap.empty
