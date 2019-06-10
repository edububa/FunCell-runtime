{-| This module contains the main program of the project. -}
module Main where

-- external imports
import Control.Concurrent
import Control.Monad (forever)
import Control.Monad.Trans.Except
import Data.Aeson (decode)
import qualified Network.WebSockets as WS
-- internal imports
import Data.Cell
import Data.ExternalModule
import Data.Messages
import Data.ServerState
import Lib.Application
import Lib.ExternalModule

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  putStrLn "[INFO]: Connection started!"
  conn <- WS.acceptRequest pending
  forever $ do
    msg <- WS.receiveData conn
    case (decode msg) :: Maybe Cell of
      Nothing   -> return ()
      Just cell -> runCell state conn cell
    case (decode msg) :: Maybe Save of
      Nothing       -> return ()
      Just (Save x) -> save state x
    case (decode msg) :: Maybe Load of
      Nothing       -> return $ Right ()
      Just (Load x) -> runExceptT $ load state conn x
    case (decode msg) :: Maybe ExternalModule of
      Nothing     -> return $ Right ()
      Just extMod -> do runExceptT $ saveAndLoadExternalModule extMod
                        updateClientCells state conn
                        return $ Right ()
main :: IO ()
main = do
  state <- newMVar newServerState
  putStr "[INFO]: Starting Server... "
  saveExternalModuleFile "module ExternalModule where\n"
  putStrLn "Done!"
  WS.runServer "127.0.0.1" 9160 $ application state


data Mes = Enero | Febrero | Marzo | Abril | Mayo | Junio | Julio | Agosto | Septiembre | Octubre | Noviembre | Diciembre deriving (Show, Eq)

data Fecha = Fecha { mes :: Mes, dia :: Int, ano :: Int }

instance Show Fecha where
  show (Fecha m d a) = show d <> " de " <> show m <> " de " <> show a

filtrarMes :: Mes -> [(Int, Fecha)] -> [(Int, Fecha)]
filtrarMes mes = filter (\(_, (Fecha m _ _)) -> mes == m)
