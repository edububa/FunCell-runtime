-- | This module contains a library of functions to deal with the
-- @ServerState@
module Lib.ServerState where

-- external imports
import Control.Concurrent
-- internal imports
import Data.Cell
import Data.ServerState
import Lib.Cell
import Lib.Dependency

-- | 'updateState' updates the state of the server with the given cell
-- and its dependencies.
updateState :: MVar ServerState -> Cell -> (Index, [Index]) -> IO ()
updateState state cell deps = do
  modifyMVar_ state $ updateCell cell
  modifyMVar_ state $ updateDependencies deps

-- | 'updateDependencies' updates the state of the dependencies of the
-- @ServerState@.
updateDependencies :: Monad m => (Index, [Index]) -> ServerState -> m ServerState
updateDependencies (from, tos) (ss, ds) = return (ss, updateDependency from tos ds)

-- | 'updateCell' updates the @ServerState@ adding the new
-- cell, or replaces it if already exists.
updateCell :: Monad m => Cell -> ServerState -> m ServerState
updateCell cell (ss, ds) = return (addCell cell ss, ds)
