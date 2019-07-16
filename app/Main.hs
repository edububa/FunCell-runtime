{-| This module contains the main program of the project. -}
module Main where

-- external imports
import Control.Concurrent
import Control.Monad (forever)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Aeson (decode)
import qualified Network.WebSockets as WS
-- internal imports
import Data.Cell
import Data.ExternalModule
import Data.Messages
import Data.ServerState
import Lib.Application
import Lib.ExternalModule
-- debug
import Debug.Trace (traceIO)

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  traceIO "[INFO]: Connection started!"
  conn <- WS.acceptRequest pending
  forever $ do
    msg <- WS.receiveData conn
    case (decode msg) :: Maybe Cell of
      Nothing   -> return ()
      Just cell -> do evalCell state conn cell
                      updateDependents state conn cell
    case (decode msg) :: Maybe Save of
      Nothing       -> return ()
      Just (Save x) -> save state x
    case (decode msg) :: Maybe Load of
      Nothing       -> return $ Right ()
      Just (Load x) -> runExceptT $ load state conn x
    case (decode msg) :: Maybe ExternalModule of
      Nothing     -> return $ Right ()
      Just extMod -> do runExceptT $ saveAndLoadExternalModule extMod
                        runMaybeT  $ updateSpreadSheet state conn
                        return $ Right ()

main :: IO ()
main = do
  state <- newMVar newServerState
  putStr "[INFO]: Starting Server... "
  saveExternalModuleFile "module ExternalModule where\n"
  putStrLn "Done!"
  WS.runServer "127.0.0.1" 9160 $ application state
