{-| This module contains the definition of the ServerState used by the
  web server.-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.ServerState where

-- internal imports
import Data.Cell
import Data.SpreadSheet as SS
import Data.Dependency as Dep

type ServerState = (SpreadSheet Cell, Dependencies)

{-| The value 'newServerstate' contains the initial value of the state
  of the server. -}
newServerState :: ServerState
newServerState = (SS.empty, Dep.empty)
