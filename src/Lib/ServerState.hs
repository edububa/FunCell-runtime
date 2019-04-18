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

updateState :: Cell -> MVar ServerState -> (Index, [Index])-> IO ()
updateState cell state deps = do
  modifyMVar_ state $ updateSpreadSheet cell
  modifyMVar_ state $ updateDeps deps

updateDeps :: Monad m => (Index, [Index]) -> ServerState -> m ServerState
updateDeps (from, tos) (ss, ds) = return (ss, updateDependency from tos ds)

updateSpreadSheet :: Monad m => Cell -> ServerState -> m ServerState
updateSpreadSheet cell (ss, ds) = return (addCell cell ss, ds)
