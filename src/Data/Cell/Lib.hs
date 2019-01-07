module Data.Cell.Lib where

import Data.Cell

import Data.Foldable (foldr)
import Data.Either (Either(..))
import Data.Map (empty, insert, lookup)
import qualified Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set, member)

-- Lib functions
createSpreadSheet :: ((Row, Col) -> a) -> Int -> Int -> SpreadSheet a
createSpreadSheet e rows columns = foldr f empty keys
    where keys = (,) <$> [0..rows] <*> [0..columns]
          f x = insert x $ e x

emptyCell :: (Row, Col) -> Cell
emptyCell (r, c) = Cell { row = r, col = c, content = mempty, evalResult = Right "" }

updateCellVal :: Row -> Col -> a -> SpreadSheet a -> SpreadSheet a
updateCellVal r c = insert (r, c)

-- updateCellContent :: Row -> Col -> String -> SpreadSheet Cell -> SpreadSheet Cell
-- updateCellContent r c s table = updateCellVal r c cell' table
--   where cell = maybe (emptyCell $ Tuple r c) id (getCell r c table)
--         cell' = Cell $ cell { content = Just s }

getCell :: Int -> Int -> SpreadSheet a -> Maybe a
getCell r c = Map.lookup (r, c)

-- getCells :: Set (Row, Col) -> SpreadSheet a -> [a]
-- getCells keys = fromFoldable . filterKeys f
--   where f k = Map.member k keys

updateCell :: Cell -> SpreadSheet Cell -> SpreadSheet Cell
updateCell c = updateCellVal (row c) (col c) c

updateCells :: SpreadSheet Cell -> [Cell] -> SpreadSheet Cell
updateCells = foldr updateCell

clearEval :: Cell -> Cell
clearEval c = c { evalResult = Right "" }

clearEvalCell :: Row -> Col -> SpreadSheet Cell -> SpreadSheet Cell
clearEvalCell r c table = maybe table f $ getCell r c table
  where f cell = updateCellVal r c (clearEval cell) table
