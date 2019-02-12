{-| This module contains some useful functions to deal with @Cell@ and
    @SpreadSheet@ -}
module Data.Cell.Lib where

import Data.SpreadSheet (SpreadSheet)
import qualified Data.SpreadSheet as SpreadSheet
import Data.Cell

empty :: SpreadSheet Cell
empty = SpreadSheet.empty

emptyCell :: Index -> Cell
emptyCell (r, c) = Cell r c Nothing (Right "")

getCell :: Index -> SpreadSheet Cell -> Cell
getCell i = maybe (emptyCell i) id . SpreadSheet.get i

addCell :: Cell -> SpreadSheet Cell -> SpreadSheet Cell
addCell c = SpreadSheet.put (getIndex c) (const c)

getIndex :: Cell -> Index
getIndex = (,) <$> row <*> col

clearCell :: Index -> SpreadSheet Cell -> SpreadSheet Cell
clearCell = addCell . emptyCell
