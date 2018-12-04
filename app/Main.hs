{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import Data.Aeson (encode, eitherDecode)
import Data.Maybe (isJust)
import Control.Monad (forever)
import qualified Network.WebSockets as WS

import Lib
import ParsingLib

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
    let cell = (eitherDecode msg) :: Either String Cell
    either
      (\x -> putStrLn $ "[ERROR]: " <> x)
      (updateEvalResult conn)
      cell
      where updateEvalResult conn cell = do
              let cell' = cell { evalResult = maybe (Right "") parseAndEval . content $ cell }
              WS.sendTextData conn $ encode cell'
              return ()

main :: IO ()
main = do
  putStr "Starting Server... "
  putStrLn "Done!"
  WS.runServer "127.0.0.1" 9160 $ application newServerState
