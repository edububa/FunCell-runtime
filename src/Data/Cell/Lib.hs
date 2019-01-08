{-| This module contains some useful functions to deal with @Cell@ and
    @SpreadSheet@ -}
module Data.Cell.Lib where

import Data.Cell

import Data.Foldable (foldr)
import Data.Either (Either(..))
import Data.Map (empty, insert, lookup)
import qualified Data.Map as Map
import Data.Maybe (Maybe(..), maybe, catMaybes)

{-| 'createSpreadSheet' takes a @Cell@ generator, the number of columns
     and the number of rows and returns the generated @SpreadSheet@. -}
createSpreadSheet :: ((Row, Col) -> a) -> Int -> Int -> SpreadSheet a
createSpreadSheet e rows columns = foldr f empty keys
    where keys = (,) <$> [0..rows] <*> [0..columns]
          f x = insert x $ e x

{-| 'emptyCell' generates an empty @Cell@. -}
emptyCell :: (Row, Col) -> Cell
emptyCell (r, c) = Cell { row = r, col = c, content = mempty, evalResult = Right "" }

{-| 'updateSpreadSheet' updates a @Cell@ in a @SpreadSheet@ -}
updateSpreadSheet :: Row -> Col -> a -> SpreadSheet a -> SpreadSheet a
updateSpreadSheet r c = insert (r, c)

-- updateCellContent :: Row -> Col -> String -> SpreadSheet Cell -> SpreadSheet Cell
-- updateCellContent r c s table = updateCellVal r c cell' table
--   where cell = maybe (emptyCell $ Tuple r c) id (getCell r c table)
--         cell' = Cell $ cell { content = Just s }

{-| 'getCell' returns a @Cell@ for the given row and column if exists. -}
getCell :: Int -> Int -> SpreadSheet a -> Maybe a
getCell r c = Map.lookup (r, c)

getCell' :: SpreadSheet a -> (Int, Int) -> Maybe a
getCell' table k = Map.lookup k table

{-| 'updateCell' is a wrapper function for 'updateSpreadSheet'. -}
updateCell :: Cell -> SpreadSheet Cell -> SpreadSheet Cell
updateCell c = updateSpreadSheet (row c) (col c) c

{-| 'updateCells' updates the values of a given @Foldable@ of @Cell@. -}
updateCells :: Foldable f => SpreadSheet Cell -> f Cell -> SpreadSheet Cell
updateCells = foldr updateCell

{-| 'clearEval' clears the @evalResult@ field in a @Cell@. -}
clearEval :: Cell -> Cell
clearEval c = c { evalResult = Right "" }

{-| 'clearEvalCell' clears a @Cell@ located by @Row@ and @Col@. -}
clearEvalCell :: Row -> Col -> SpreadSheet Cell -> SpreadSheet Cell
clearEvalCell r c table = maybe table f $ getCell r c table
  where f cell = updateCell (clearEval cell) table
