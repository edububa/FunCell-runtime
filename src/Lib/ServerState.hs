{-| This module contains a library of functions to deal with the
  @ServerState@ -}
module Lib.ServerState where

-- external imports
import Control.Concurrent
-- internal imports
import Data.Cell
import Data.ServerState
import Lib.Cell
import Lib.Dependency

{-| 'updateState' updates the state of the server with the given cell
  and its dependencies. -}
updateState :: Cell -> MVar ServerState -> (Index, [Index]) -> IO ()
updateState cell state deps = do
  modifyMVar_ state $ updateSpreadSheet cell
  modifyMVar_ state $ updateDeps deps

{-| 'updateDeps' updates the state of the dependencies of the
  @ServerState@. -}
updateDeps :: Monad m => (Index, [Index]) -> ServerState -> m ServerState
updateDeps (from, tos) (ss, ds) = return (ss, updateDependency from tos ds)

{-| 'updateSpreadSheet' updates the @ServerState@ adding the new cell, or
  replaces it if already exists. -}
updateSpreadSheet :: Monad m => Cell -> ServerState -> m ServerState
updateSpreadSheet cell (ss, ds) = return (addCell cell ss, ds)
