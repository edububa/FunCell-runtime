module Main where

import Data.Text (Text)
import Data.Aeson (ToJSON, encode, eitherDecode, decode)
import Data.Either (isRight, fromRight)
import Data.Maybe (isJust, isNothing)
import Data.SpreadSheet (SpreadSheet)
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
import Lib.Dependency (analyzeCircularDependencies, Dependencies, addDependencies', getDependencies)
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
      Just cell -> runExceptT $ runCell state cell conn
      Nothing -> return $ Left ""
    case (decode msg) :: Maybe ExternalModule of
      Just extMod -> runExceptT $ saveAndLoadExternalModule extMod -- TODO update used cells
      Nothing -> return $ Right ()

runCell :: MVar ServerState -> Cell -> WS.Connection -> ExceptT Error IO Cell
runCell state cell conn = do
  deps  <- analyzeDependencies state cell -- TODO not working correctly
  cell' <- eval state cell
  liftIO $ putStrLn $ show deps
  liftIO $ sendResult conn cell'
  liftIO $ updateState cell deps state
  return cell'

analyzeDependencies :: MVar ServerState -> Cell -> ExceptT Error IO (Index, [Index])
analyzeDependencies state cell = ExceptT $ do
  (ss, deps) <- liftIO $ readMVar state
  let i    = getIndex cell
      refs = maybe [] parseReferences $ content cell
  liftIO $ putStrLn $ show refs
  case analyzeCircularDependencies (i:refs) deps of
    Nothing -> return $ Right (i, refs)
    Just x  -> return $ Left (show x)

eval :: MVar ServerState -> Cell -> ExceptT Error IO Cell
eval state cell = do
  case content cell of
    Nothing -> return cell
    Just c  -> do
      s   <- liftIO $ readMVar state
      c'  <- except $ solveDependencies (fst s) c
      res <- liftIO . runExceptT . evalCell $ c'
      return cell { evalResult = res }

updateState :: Cell -> (Index, [Index]) -> MVar ServerState -> IO ()
updateState cell (i, is) state = do
  (ss, ds) <- readMVar state
  modifyMVar_ state $ updateSpreadSheet cell
  modifyMVar_ state $ updateDeps i is

updateDeps :: Monad m => Index -> [Index] -> ServerState -> m ServerState
updateDeps to froms (ss, ds) = return (ss, addDependencies' to froms ds)

updateSpreadSheet :: Monad m => Cell -> ServerState -> m ServerState
updateSpreadSheet cell (ss, ds) = return (addCell cell ss, ds)

sendResult :: ToJSON a => WS.Connection -> a -> IO ()
sendResult conn = WS.sendTextData conn . encode

main :: IO ()
main = do
  state <- newMVar newServerState
  writeFile "ExternalModule" ""
  putStr "Starting Server... "
  putStrLn "Done!"
  WS.runServer "127.0.0.1" 9160 $ application state
