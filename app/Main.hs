{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import Data.Aeson (encode, eitherDecode)
import Data.Maybe (isJust)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import qualified Network.WebSockets as WS

import Lib
import EvalLib

type ServerState = [Text]

newServerState :: ServerState
newServerState = []

application :: ServerState -> WS.ServerApp
application state pending = do
  putStrLn "Connection started!"
  conn <- WS.acceptRequest pending
  forever $ do
    msg  <- WS.receiveData conn
    putStrLn $ "[RECEIVED]: " <> (show msg)
    case (eitherDecode msg) :: Either String Cell of
      Left  x -> putStrLn $ "[ERROR]: " <> x
      Right x -> evalAndSendCell conn x

evalAndSendCell :: WS.Connection -> Cell -> IO ()
evalAndSendCell conn cell = do
  case content cell of
    Nothing -> return ()
    Just  x -> do
      res <- parseAndEval x
      WS.sendTextData conn . encode $ cell { evalResult = res }

main :: IO ()
main = do
  putStr "Starting Server... "
  putStrLn "Done!"
  WS.runServer "127.0.0.1" 9160 $ application newServerState
