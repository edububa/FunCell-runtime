module Lib.Eval where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Either (partitionEithers, rights)
import Data.Either.Combinators (mapLeft)
import Data.Maybe (catMaybes)
import Data.SpreadSheet (SpreadSheet)
import Data.String.Utils (replace)
import Language.Haskell.Interpreter as I
import Text.ParserCombinators.ReadP

import Lib.Indexing
import Data.Cell
import Data.Cell.Lib
import Data.ExternalModule

type Error = String

context :: InterpreterT IO ()
context = do
  I.loadModules ["ExternalModule.hs"]
  setImports [ "Prelude"
             , "Data.SpreadSheet.Date"
             , "ExternalModule"
             , "Data.SpreadSheet"
             , "Data.SpreadSheet.Cell"
             , "Data.Function"]

evalCell :: [Char] -> ExceptT Error IO String
evalCell ""    = return ""
evalCell input = ExceptT $ do
  typeRes <- liftIO $ I.runInterpreter $ do { context; typeChecks input }
  evalRes <- liftIO $ I.runInterpreter $ do { context; eval input }
  return $ case (typeRes, evalRes) of
    (Right False, _)      -> Left "Won't compile"
    (Right True, Left _)  -> Left "Not showable"
    (Right True, Right x) -> Right x
    _                     -> Left "Unknown error"

solveDependencies :: SpreadSheet Cell -> String -> Either Error String
solveDependencies state xs =
  case parseReferences xs of
    [] -> Right xs
    indices -> solveDependencies state . foldr applyValues xs . rights .
               fmap (cellToIndexAndVal . flip getCell state) $ indices

applyValues :: (String, String) -> String -> String
applyValues (from, to) = replace from to

cellToIndexAndVal :: Cell -> Either Error (String, String)
cellToIndexAndVal cell = do
  case content cell of
    Nothing -> Left $ "Error in cell " <> index
    Just x  -> Right (index, "(" <> x <> ")")
  where index = intToCol (col cell) <> intToRow (row cell)

saveAndLoadExternalModule :: ExternalModule -> ExceptT Error IO ()
saveAndLoadExternalModule (ExternalModule input) = ExceptT $ do
  liftIO $ saveExternalModuleFile input
  res <- liftIO $ runInterpreter $ I.loadModules ["ExternalModule.hs"]
  return . mapLeft (const "Won't compile") $ res

saveExternalModuleFile :: String -> IO ()
saveExternalModuleFile input = do
  writeFile "ExternalModule.hs" ("module ExternalModule where \n" <> input)
