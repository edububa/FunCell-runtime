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
import Lib.Eval
import Lib.Indexing
import Lib.Dependency
import Lib.ExternalModule

runCell :: MVar ServerState -> WS.Connection -> Cell -> IO ()
runCell state conn cell = do
  res <- runExceptT $ runEval state cell
  let cell' = cell { evalResult = res }
  sendResult conn cell'
  -- updateState cell' state TODO

runEval :: MVar ServerState -> Cell -> ExceptT Error IO String
runEval state cell = do
  _ <- analyzeDependencies state cell
  res  <- eval state cell
  return res

analyzeDependencies :: MVar ServerState -> Cell -> ExceptT Error IO (Index, [Index])
analyzeDependencies state cell = ExceptT $ do
  (_, deps) <- liftIO $ readMVar state
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

sendResult :: ToJSON a => WS.Connection -> a -> IO ()
sendResult conn = WS.sendTextData conn . encode
