-- | This module contains functions to handle the evaluation of the
-- content of cells.
module Lib.Eval
  ( -- * Evaluation Functions
    context
  , eval
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
import Language.Haskell.Interpreter hiding (eval)
import qualified Language.Haskell.Interpreter as I (eval)
-- internal imports
import Data.Cell
import Lib.Cell
import Lib.Indexing
import Lib.Parsing

-- | 'context' configures the modules and the environment of the cell
-- evaluation. The loaded modules are:
--
-- - Prelude
-- - ExternalModule
-- - Data.SpreadSheet
-- - Data.SpreadSheet.Cell
-- - Data.Function
context :: InterpreterT IO ()
context = do
  loadModules ["ExternalModule.hs"]
  setImports [ "Prelude"
             , "ExternalModule"
             , "Data.SpreadSheet.Date"
             , "Data.SpreadSheet"
             , "Data.SpreadSheet.Cell"
             , "Data.Function"
             , "Data.Either" ]

-- | 'eval' typechecks and evals the content of a cell.
--
-- >>> runExceptT $ eval "1 + 2"
-- Right "3"
-- >>> runExceptT $ eval "1 + True"
-- Left "Won't compile"
eval :: String -> ExceptT Error IO String
eval ""    = return ""
eval input = ExceptT $ do
  typeRes <- liftIO $ runInterpreter $ do { context; typeChecks input }
  evalRes <- liftIO $ runInterpreter $ do { context; I.eval input }
  return $ case (typeRes, evalRes) of
    (Right False, _)      -> Left $ "eval error;\n\t - expression: " <> input
    (Right True, Left _)  -> Left $ "not showable;\n\t - expression: " <> input
    (Right True, Right x) -> Right x
    _                     -> Left $ "unknown error;\n\t expression: " <> input

-- | 'solveDependencies' returns the string with the cell dependencies
-- solved. The @[Index]@ input must be in topological order.
solveDependencies :: SpreadSheet Cell -> [Index] -> String -> Either Error String
solveDependencies _ _ "" = Right ""
solveDependencies _ [] xs = Right xs
solveDependencies state is _ = do
  let is' = reverse is          -- the last element is the string we want to eval
      vs  = fmap (cellToIndexAndVal . flip getCell state) is'
  case lefts vs of
    []  -> Right $ applyValues . rights $ vs
    err -> Left  $ foldr (<>) [] err

-- | 'applyValues'
applyValues :: [(String, String)] -> String
applyValues [] = []             -- this case should never happen
applyValues (x:[]) = snd x
applyValues (x:xs) = applyValues $ (fmap . fmap) (applyValue x) xs

-- | 'applyValue'
applyValue :: (String, String) -> String -> String
applyValue (from, to) = replace from to

-- | 'cellToIndexAndVal'
cellToIndexAndVal :: Cell -> Either Error (String, String)
cellToIndexAndVal cell = do
  c <- case content cell of
         Nothing -> Left $ "Error empty cell " <> index
         Just "" -> Left $ "Error empty cell " <> index
         Just x  -> Right x
  c' <- desugarContent c
  return (index, "(" <> c' <> ")")
  where index = intToCol (col cell) <> intToRow (row cell)
