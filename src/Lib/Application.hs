{-| This module contains a library of functions used by the main
  application of the project -}
module Lib.Application where

-- external imports
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
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

-- | 'runCell'
runCell :: MVar ServerState -> WS.Connection -> Cell -> IO ()
runCell state conn cell = do
  res <- runExceptT $ runEval state cell
  let cell' = cell { evalResult = res }
  sendClient conn cell'
  updateDependentCells state conn cell'
  liftIO $ modifyMVar_ state $ updateSpreadSheet cell'

-- | 'runEval'
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

-- | 'updateDependentCells'
updateDependentCells :: MVar ServerState -> WS.Connection -> Cell -> IO ()
updateDependentCells state conn cell = do
  (ss, deps) <- readMVar state
  let refs = getDependents (getIndex cell) deps
  mapM_ (runCell state conn . flip getCell ss) refs

-- | 'analyzeDependencies'
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

-- | 'eval'
eval :: MVar ServerState -> Cell -> ExceptT Error IO String
eval _          (Cell { content = Nothing }) = return ""
eval state cell@(Cell { content = Just c  }) = do
  s   <- liftIO $ readMVar state
  let deps = getDependencies (getIndex cell) (snd s)
  c'  <- except $ solveDependencies (fst s) deps c
  liftIO $ putStrLn $ "[EVAL]: " <> c'
  res <- liftIO . runExceptT . evalCell $ c'
  let cell' = cell { evalResult = res }
  liftIO $ modifyMVar_ state $ updateSpreadSheet cell'
  except res

-- | 'sendClient' encodes and sends data that can be converted to
-- JSON through an open WebSocket connection.
sendClient :: ToJSON a => WS.Connection -> a -> IO ()
sendClient conn = WS.sendTextData conn . encode

-- | updateClientCells
updateClientCells :: MVar ServerState -> WS.Connection -> IO ()
updateClientCells state conn = do
  (ss, _) <- readMVar state
  liftIO $ mapM_ (runCell state conn) (toListValues ss)

save :: MVar ServerState -> String -> IO ()
save state path = do
  s <- readMVar state
  file <- readFile "ExternalModule.hs" -- unsafe
  encodeFile path (s, ExternalModule file)
  putStrLn $ "[INFO]: saved file: '" <> path  <> "'"

load :: MVar ServerState -> WS.Connection -> String -> ExceptT Error IO ()
load state conn path = do
  file <- liftIO (eitherDecodeFileStrict' path :: IO (Either Error (ServerState, ExternalModule)))
  s    <- except $ file
  liftIO $ modifyMVar_ state $ \_ -> return $ fst s
  liftIO $ putStrLn $ "[INFO]: loaded file: '" <> path <> "'"
  saveAndLoadExternalModule $ snd s
  liftIO $ sendClient conn $ snd s
  liftIO $ mapM_ (sendClient conn) (toListValues $ fst . fst $ s)
  liftIO $ updateClientCells state conn
