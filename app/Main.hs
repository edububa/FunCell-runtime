{-| This module contains the main program of the project. -}
module Main where

-- external imports
import Control.Concurrent
import Control.Monad (forever)
import Control.Monad.Trans.Except
import Data.Aeson (decode)
import qualified Network.WebSockets as WS
-- internal imports
import Data.Cell
import Data.ExternalModule
import Data.ServerState
import Lib.Application
import Lib.ExternalModule

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  putStrLn "Connection started!"
  conn <- WS.acceptRequest pending
  forever $ do
    msg <- WS.receiveData conn
    case (decode msg) :: Maybe Cell of
      Nothing   -> return ()
      Just cell -> runCell state conn cell
    case (decode msg) :: Maybe ExternalModule of
      Nothing -> return $ Right ()
      Just extMod -> runExceptT $ runExternalModule extMod state conn

main :: IO ()
main = do
  state <- newMVar newServerState
  writeFile "ExternalModule" ""
  putStr "Starting Server... "
  saveExternalModuleFile ""
  putStrLn "Done!"
  WS.runServer "127.0.0.1" 9160 $ application state
