module Lib.Eval where

import Data.Either (partitionEithers, rights)
import Data.Either.Combinators (mapLeft)
import Data.Maybe (catMaybes)
import Data.String.Utils (replace)
import Language.Haskell.Interpreter as I
import Text.ParserCombinators.ReadP

import Lib.Indexing
import Data.Cell
import Data.Cell.Lib

type Error = String

evalCell ""    = return $ Right ""
evalCell input = do
  res <- I.runInterpreter $ do { setImports ["Prelude"]; eval input}
  return $ mapLeft (const "Type error") res

solveDependencies :: String -> SpreadSheet Cell -> Either Error String
solveDependencies xs state =
  case parseReferences xs of
    [] -> Right xs
    indices -> Right $ foldr applyValues xs . rights .
               fmap cellToIndexAndVal . catMaybes .
               fmap (getCell' state) $ indices


applyValues :: (String, String) -> String -> String
applyValues (from, to) = replace from to

cellToIndexAndVal :: Cell -> Either Error (String, String)
cellToIndexAndVal cell = do
  case evalResult cell of
    Left  x -> Left $ "Error in cell " <> index
    Right x -> Right (index, x)
  where index = intToCol (col cell) <> intToRow (row cell)
  -- (intToCol (col cell) <> intToRow (row cell),
  -- evalResult cell)
