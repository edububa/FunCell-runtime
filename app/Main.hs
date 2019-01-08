module Main where

import Data.Text (Text)
import Data.Aeson (encode, eitherDecode)
import Data.Maybe (isJust)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import qualified Network.WebSockets as WS

import Data.Cell.Lib
import Data.Cell
import Lib.Eval

type ServerState = SpreadSheet Cell

nCols :: Int
nCols = length ['A'..'Z']

nRows :: Int
nRows = 20

newServerState :: ServerState
newServerState = createSpreadSheet emptyCell nCols 20

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  putStrLn "Connection started!"
  conn <- WS.acceptRequest pending
  forever $ do
    msg  <- WS.receiveData conn
    -- putStrLn $ "[RECEIVED]: " <> (show msg)
    case (eitherDecode msg) :: Either String Cell of
      Left  x    -> putStrLn $ "[ERROR]: " <> x
      Right cell -> evalUpdateAndSend conn state cell

modifyServerState :: Monad m => Cell -> SpreadSheet Cell -> m (SpreadSheet Cell)
modifyServerState cell s = return $ updateCell cell s

evalUpdateAndSend :: WS.Connection -> MVar (SpreadSheet Cell) -> Cell -> IO ()
evalUpdateAndSend conn state cell = do
  putStrLn $ "[RECEIVED]: \t\t" <> show cell
  case content cell of
    Nothing -> return ()
    Just  x -> do
      s   <- readMVar state
      let (Right y) = solveDependencies x s
      putStrLn $ "[SOLVED_DEPENDENCIES]: \t" <> y
      res <- evalCell y
      putStrLn $ "[EVAL_RESULT]: \t" <> (show res)
      let cell' = cell { evalResult = res }
      modifyMVar_ state $ modifyServerState cell'
      WS.sendTextData conn . encode $ cell'

main :: IO ()
main = do
  state <- newMVar newServerState
  putStr "Starting Server... "
  putStrLn "Done!"
  WS.runServer "127.0.0.1" 9160 $ application state
