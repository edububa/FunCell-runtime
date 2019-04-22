{-| This module contains a library of functions used by the main
  application of the project -}
module Lib.Application where

-- external imports
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Aeson (ToJSON, encode)
import Data.SpreadSheet (toListValues)
import qualified Network.WebSockets as WS
-- internal imports
import Data.Cell
import Data.ServerState
import Data.ExternalModule
import Lib.Cell
import Lib.Dependency
import Lib.Eval
import Lib.ExternalModule
import Lib.Indexing
import Lib.Parsing
import Lib.ServerState

runCell :: MVar ServerState -> WS.Connection -> Cell -> IO ()
runCell state conn cell = do
  res <- runExceptT $ runEval state cell
  let cell' = cell { evalResult = res }
  sendResult conn cell'
  modifyMVar_ state $ updateSpreadSheet cell'
  updateDependentCells state conn cell'

runEval :: MVar ServerState -> Cell -> ExceptT Error IO String
runEval _     (Cell { content = Nothing }) = return ""
runEval state cell@(Cell { content = Just c }) = do
  c'   <- except $ desugarContent c
  let cell' = cell { content = Just c' }
  deps <- analyzeDependencies state cell'
  liftIO $ updateState cell' state deps
  res  <- eval state cell'
  liftIO $ putStrLn $ "[RES]:  " <> res
  return res

updateDependentCells :: MVar ServerState -> WS.Connection -> Cell -> IO ()
updateDependentCells state conn cell = do
  (ss, deps) <- readMVar state
  let refs = getDependents (getIndex cell) deps
  mapM_ (runCell state conn . flip getCell ss) refs

analyzeDependencies :: MVar ServerState -> Cell -> ExceptT Error IO (Index, [Index])
analyzeDependencies _     cell@(Cell { content = Nothing }) = pure (getIndex cell, [])
analyzeDependencies state cell@(Cell { content = Just c  }) = do
  (_, deps) <- liftIO $ readMVar state
  let i    = getIndex cell
      refs = parseReferences c
  except $
    if circularDependencies $ addDependencies i refs deps
    then Left  ("Circular dependencies found")
    else Right (i, refs)

eval :: MVar ServerState -> Cell -> ExceptT Error IO String
eval _      (Cell { content = Nothing }) = return ""
eval s cell@(Cell { content = Just c  }) = do
  state <- liftIO $ readMVar s
  let deps = getDependencies (getIndex cell) (snd state)
  c' <- except $ solveDependencies (fst state) deps c
  liftIO $ putStrLn $ "[EVAL]: " <> c'
  res <- liftIO . runExceptT . evalCell $ c'
  except res

runExternalModule :: ExternalModule -> MVar ServerState -> WS.Connection -> ExceptT Error IO ()
runExternalModule extMod state conn = do
  saveAndLoadExternalModule extMod
  (ss, _) <- liftIO $ readMVar state
  liftIO $ mapM_ (runCell state conn) (toListValues ss)

sendResult :: ToJSON a => WS.Connection -> a -> IO ()
sendResult conn = WS.sendTextData conn . encode
