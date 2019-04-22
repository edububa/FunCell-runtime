{-| This module contains functions to handle the evaluation of the
  content of cells. -}
module Lib.Eval
  ( -- * Evaluation Functions
    context
  , evalCell
    -- * Dependency Solving Functions
  , solveDependencies
    -- ** Auxiliar Functions
  , applyValues
  , applyValue
  , cellToIndexAndVal
  ) where

-- external imports
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Either (rights, lefts)
import Data.SpreadSheet (SpreadSheet)
import Data.String.Utils (replace)
import Language.Haskell.Interpreter as I
-- internal imports
import Data.Cell
import Lib.Indexing
import Lib.Cell

{-| 'context' -}
context :: InterpreterT IO ()
context = do
  I.loadModules ["ExternalModule.hs"]
  setImports [ "Prelude"
             , "Data.SpreadSheet.Date"
             , "ExternalModule"
             , "Data.SpreadSheet"
             , "Data.SpreadSheet.Cell"
             , "Data.Function"]

{-| 'evalCell' -}
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

{-| 'solveDependencies' -}
solveDependencies :: SpreadSheet Cell -> [Index] -> String -> Either Error String
solveDependencies _ [] xs = Right xs
solveDependencies state is _ = do
  let is' = reverse is          -- the last element is the string we want to eval
      vs  = fmap (cellToIndexAndVal . flip getCell state) is'
  case lefts vs of
    []  -> Right $ applyValues . rights $ vs
    err -> Left  $ foldr (<>) [] err

{-| 'applyValues' -}
applyValues :: [(String, String)] -> String
applyValues [] = []             -- this case should never happen
applyValues (x:[]) = snd x
applyValues (x:xs) = applyValues $ (fmap . fmap) (applyValue x) xs

{-| 'applyValue' -}
applyValue :: (String, String) -> String -> String
applyValue (from, to) = replace from to

{-| 'cellToIndexAndVal' -}
cellToIndexAndVal :: Cell -> Either Error (String, String)
cellToIndexAndVal cell =
  case content cell of
    Nothing -> Left $ "Error empty cell " <> index
    Just "" -> Left $ "Error empty cell " <> index
    Just x  -> Right (index, "(" <> x <> ")")
  where index = intToCol (col cell) <> intToRow (row cell)
