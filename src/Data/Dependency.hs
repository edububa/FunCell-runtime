{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-| This module contains the definition of the Dependency data type. -}
module Data.Dependency where

-- external imports
import Algebra.Graph.AdjacencyMap
import Algebra.Graph.AdjacencyMap.Internal
import Data.Aeson
import GHC.Generics
-- internal imports
import Data.Cell

{-| 'Dependencies' will be stored in a directed graph. -}
type Dependencies = AdjacencyMap Index

{-| The 'empty' value of an empty dependency graph. -}
empty :: Dependencies
empty = Algebra.Graph.AdjacencyMap.empty

deriving instance Generic (AdjacencyMap a)

instance (ToJSON a, ToJSONKey a) => ToJSON (AdjacencyMap a)

instance (Ord a, FromJSON a, FromJSONKey a) => FromJSON (AdjacencyMap a)
