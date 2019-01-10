{-| This module contains some useful functions to deal with @Cell@ and
    @SpreadSheet@ -}
module Data.Cell.Lib where

import qualified Data.Map as Map
import Data.Cell

empty :: SpreadSheet Cell
empty = Map.empty

emptyCell :: Index -> Cell
emptyCell (r, c) = Cell r c Nothing (Right "")

getCell :: Index -> SpreadSheet Cell -> Cell
getCell i = maybe (emptyCell i) id . Map.lookup i

addCell :: Cell -> SpreadSheet Cell -> SpreadSheet Cell
addCell c = Map.insert (getIndex c) c

getIndex :: Cell -> Index
getIndex = (,) <$> row <*> col

clearCell :: Index -> SpreadSheet Cell -> SpreadSheet Cell
clearCell = addCell . emptyCell
