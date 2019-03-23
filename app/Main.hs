module Main where

import Data.Text (Text)
import Data.Aeson (ToJSON, encode, eitherDecode)
import Data.Either (isRight, fromRight)
import Data.Maybe (isJust)
import Data.SpreadSheet (SpreadSheet)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO)
import qualified Network.WebSockets as WS
import System.Timeout

import Data.Cell.Lib
import Data.Cell
import Data.ExternalModule
import Lib.Eval
import Lib.Dependency (Dependencies, addDependency, getDependencies, addDependencies')
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
    let cell   = (eitherDecode msg) :: Either String Cell
        extMod = (eitherDecode msg) :: Either String ExternalModule
    mapM_ (saveAndLoadExternalModule) extMod
    mapM_ (evalUpdateAndSend conn state) cell

evalUpdateAndSend :: WS.Connection -> MVar ServerState -> Cell -> IO ()
evalUpdateAndSend conn state cell = do
  case content cell of
    Nothing -> return ()
    Just  x -> do
      s   <- readMVar state
      res <- solveDepAndEval x s
      let cell' = cell { evalResult = res }
          ds    = getDependencies (getIndex cell) (snd s)
      sendResult conn cell'
      putStrLn $ "Cell: " <> (show cell')
      putStrLn $ "Deps: " <> (show $ snd s) <> "\n"
      modifyMVar_ state $ updateCell cell'
      modifyMVar_ state $ updateDeps (getIndex cell') (parseReferences x)
      evalDeps conn state (getIndex cell')

evalDeps :: WS.Connection -> MVar ServerState -> Index -> IO ()
evalDeps conn state i = do
  s <- readMVar state
  let cs = fmap (flip getCell . fst $ s) (getDependencies i $ snd s)
  mapM_ (evalUpdateAndSend conn state) cs

sendResult :: ToJSON a => WS.Connection -> a -> IO ()
sendResult conn = WS.sendTextData conn . encode

solveDepAndEval :: String -> ServerState -> IO (Either Error String)
solveDepAndEval input (state, _) = do
  evalResult <- evalCell res
  return evalResult
  where (Right res) = solveDependencies input state -- unsafe

updateDeps :: Monad m => Index -> [Index] -> ServerState -> m ServerState
updateDeps to froms (ss, ds) = return (ss, addDependencies' to froms ds)

updateCell :: Monad m => Cell -> ServerState -> m ServerState
updateCell cell (ss, ds) = return (addCell cell ss, ds)

main :: IO ()
main = do
  state <- newMVar newServerState
  writeFile "ExternalModule" ""
  putStr "Starting Server... "
  putStrLn "Done!"
  WS.runServer "127.0.0.1" 9160 $ application state
