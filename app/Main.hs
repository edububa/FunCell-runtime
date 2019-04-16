module Main where

import Data.Text (Text)
import Data.Aeson (ToJSON, encode, eitherDecode, decode)
import Data.Either (isRight, fromRight)
import Data.List (delete)
import Data.Maybe (isJust, isNothing)
import Data.SpreadSheet (SpreadSheet, toListValues)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import qualified Network.WebSockets as WS
import System.Timeout

import Data.Cell.Lib
import Data.Cell
import Data.ExternalModule
import Lib.Eval
import Lib.Dependency hiding (empty)
import Lib.Indexing (parseReferences)
import qualified Lib.Dependency as Dep

type ServerState = (SpreadSheet Cell, Dependencies)

newServerState :: ServerState
newServerState = (empty, Dep.empty)

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
    (ss, ds) <- readMVar state
    print ds

runCell :: MVar ServerState -> WS.Connection -> Cell -> IO ()
runCell state conn cell = do
  res <- runExceptT $ runEval state cell
  let cell' = cell { evalResult = res }
  sendResult conn cell'
  updateState cell' state conn

runEval :: MVar ServerState -> Cell -> ExceptT Error IO String
runEval state cell = do
  deps <- analyzeDependencies state cell
  res  <- eval state cell
  return res

analyzeDependencies :: MVar ServerState -> Cell -> ExceptT Error IO (Index, [Index])
analyzeDependencies state cell = ExceptT $ do
  (ss, deps) <- liftIO $ readMVar state
  let i    = getIndex cell
      refs = maybe [] parseReferences $ content cell
  if circularDependencies $ addDependencies i refs deps
     then pure $ Left  ("Circular dependencies found")
     else pure $ Right (i, refs)

eval :: MVar ServerState -> Cell -> ExceptT Error IO String
eval state cell = do
  case content cell of
    Nothing -> return ""
    Just c  -> do
      s   <- liftIO $ readMVar state
      c'  <- except $ solveDependencies (fst s) c
      res <- liftIO . runExceptT . evalCell $ c'
      except res

runExternalModule :: ExternalModule -> MVar ServerState -> WS.Connection -> ExceptT Error IO ()
runExternalModule extMod state conn = do
  saveAndLoadExternalModule extMod
  (ss, _) <- liftIO $ readMVar state
  liftIO $ mapM_ (runCell state conn) (toListValues ss)

updateState :: Cell -> MVar ServerState -> WS.Connection -> IO ()
updateState cell state conn = do
  (ss, ds) <- readMVar state
  deps <- runExceptT $ analyzeDependencies state cell
  modifyMVar_ state $ updateSpreadSheet cell
  mapM_ (modifyMVar_ state . updateDeps) deps

updateDeps :: Monad m => (Index, [Index]) -> ServerState -> m ServerState
updateDeps (from, tos) (ss, ds) = return (ss, ds')
    where ds' = addDependencies from tos $ resetDependency from ds

updateSpreadSheet :: Monad m => Cell -> ServerState -> m ServerState
updateSpreadSheet cell (ss, ds) = return (addCell cell ss, ds)

sendResult :: ToJSON a => WS.Connection -> a -> IO ()
sendResult conn = WS.sendTextData conn . encode

main :: IO ()
main = do
  state <- newMVar newServerState
  writeFile "ExternalModule" ""
  putStr "Starting Server... "
  saveExternalModuleFile ""
  putStrLn "Done!"
  WS.runServer "127.0.0.1" 9160 $ application state
