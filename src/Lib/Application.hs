{-| This module contains a library of functions used by the main
  application of the project -}
module Lib.Application where

-- external imports
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.SpreadSheet (toListValues)
import qualified Network.WebSockets as WS
-- internal imports
import Data.Cell
import Data.ExternalModule
import Data.ServerState
import Lib.Cell
import Lib.Dependency
import Lib.Eval
import Lib.ExternalModule
import Lib.Indexing
import Lib.Parsing
import Lib.ServerState
import System.Directory
-- debug
import Debug.Trace (traceIO)

-- | 'evalCell' evaluates the content of the received cell and sends
-- it back to the server.
evalCell :: MVar ServerState -> WS.Connection -> Cell -> IO ()
evalCell state conn cell = do
  res <- runExceptT $ evalContent state cell
  let cell' = cell { evalResult = res }
  send conn cell'
  modifyMVar_ state $ updateCell cell'

-- | 'save' saves the actual state of the server in the received path.
save :: MVar ServerState -> String -> IO ()
save state path = do
  s <- readMVar state
  file <- readFile "ExternalModule.hs" -- unsafe
  encodeFile path (s, ExternalModule file)
  traceIO $ "[INFO]: saved file: '" <> path  <> "'"

-- | 'load' loads the state saved in the file received as argument.
load :: MVar ServerState -> WS.Connection -> String -> ExceptT Error IO ()
load state conn path = do
  exists <- liftIO $ doesFileExist path
  when (not exists) $ liftIO $ traceIO
    $ "[ERROR]: file: '" <> path <> "' not found."
  guard exists
  file <- liftIO (eitherDecodeFileStrict' path
                  :: IO (Either Error (ServerState, ExternalModule)))
  s    <- except file
  liftIO $ modifyMVar_ state $ \_ -> return $ fst s
  saveAndLoadExternalModule $ snd s
  liftIO $ send conn $ snd s
  liftIO $ mapM_ (send conn) (toListValues $ fst . fst $ s)
  maybeToExceptT "" $ updateSpreadSheet state conn
  liftIO $ traceIO $ "[INFO]: loaded file: '" <> path <> "'"

-- | 'updateDependents' evaluates all the dependent cells of the
-- argument cell.
updateDependents :: MVar ServerState -> WS.Connection -> Cell -> IO ()
updateDependents state conn cell = do
  (ss, deps) <- readMVar state
  let refs = getDependents (getIndex cell) deps
  mapM_ (evalCell state conn . flip getCell ss) refs

-- | 'updateSpreadSheet' evaluates all the cells with content in the
-- spreadsheet. Follows the topological order of the dependency graph.
updateSpreadSheet :: MVar ServerState -> WS.Connection -> MaybeT IO ()
updateSpreadSheet state conn = do
  (ss, ds) <- liftIO $ readMVar state
  is <- MaybeT . return $ getOrder ds
  liftIO . mapM_ (evalCell state conn) . map (flip getCell ss) $ reverse is

-- | 'evalContent' evaluates the content of the given cell. First
-- analyzes its dependencies, if no problems are found, evaluates the
-- content of the cell.
evalContent :: MVar ServerState -> Cell -> ExceptT Error IO String
evalContent _          (Cell { content = Nothing }) = return ""
evalContent state cell@(Cell { content = Just c  }) = do
  desugaredContent <- except $ desugarContent c
  let cell' = cell { content = Just desugaredContent }
  deps <- analyzeDependencies state cell'
  liftIO $ updateState state cell' deps
  (ss, ds) <- liftIO $ readMVar state
  let dependencies = getDependencies (fst deps) ds
  solvedContent <- except $ solveDependencies ss dependencies desugaredContent
  liftIO $ traceIO $ "[EVAL]: content - " <> solvedContent
  result <- eval solvedContent
  liftIO $ traceIO $ "[EVAL]: result  - " <> result
  return result

-- | 'analyzeDependencies' looks for circular references.
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

-- | 'send' encodes and sends data that can be converted to
-- JSON through an open WebSocket connection.
send :: ToJSON a => WS.Connection -> a -> IO ()
send conn = WS.sendTextData conn . encode
