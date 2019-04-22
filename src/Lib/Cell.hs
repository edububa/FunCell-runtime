{-| This module contains some useful functions to deal with @Cell@ and
    @SpreadSheet@ -}
module Lib.Cell where

-- external imports
import Data.SpreadSheet (SpreadSheet)
import qualified Data.SpreadSheet as SpreadSheet
-- internal imports
import Data.Cell
import Lib.Indexing

{-| The 'empty' value of a SpreadSheet of Cells. -}
empty :: SpreadSheet Cell
empty = SpreadSheet.empty

{-| 'emptyCell' is a function that creates an empty cell in a given
  index. -}
emptyCell :: Index -> Cell
emptyCell (r, c) = Cell r c Nothing (Right "")

{-| 'getCell' returns a @Cell@ in the index of a given @SpreadSheet@. -}
getCell :: Index -> SpreadSheet Cell -> Cell
getCell i = maybe (emptyCell i) id . SpreadSheet.get i

{-| 'addCell' inserts a cell in a @SpreadSheet@ -}
addCell :: Cell -> SpreadSheet Cell -> SpreadSheet Cell
addCell c = SpreadSheet.put (getIndex c) (const c)

{-| 'clearCell' deletes the content of the cell in a given index of a
  @SpreadSheet@ -}
clearCell :: Index -> SpreadSheet Cell -> SpreadSheet Cell
clearCell = addCell . emptyCell
