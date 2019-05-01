-- | This module contains some useful functions to deal with @Cell@
-- and @SpreadSheet@
module Lib.Cell where

-- external imports
import Data.SpreadSheet (SpreadSheet)
import qualified Data.SpreadSheet as SpreadSheet
-- internal imports
import Data.Cell
import Lib.Indexing

-- | The 'empty' value of a SpreadSheet of Cells. -}
empty :: SpreadSheet Cell
empty = SpreadSheet.empty

-- | 'emptyCell' is a function that creates an empty cell in a given
-- index.
--
-- >>> emptyCell (0,0)
-- (0, 0): Nothing Right ""
emptyCell :: Index -> Cell
emptyCell (r, c) = Cell r c Nothing (Right "")

-- | 'addCell' inserts a cell in a @SpreadSheet@.
--
-- >>> c = Cell 0 0 (Just "1 + 2") (Right "3")
-- >>> addCell c empty
-- Range (0,0) (0,0)
-- fromList [((0,0),(0, 0): Just "1 + 2" Right "3")]
addCell :: Cell -> SpreadSheet Cell -> SpreadSheet Cell
addCell c = SpreadSheet.put (getIndex c) (const c)

-- | 'getCell' returns a @Cell@ in the index of a given @SpreadSheet@.
getCell :: Index -> SpreadSheet Cell -> Cell
getCell i = maybe (emptyCell i) id . SpreadSheet.get i

-- | 'clearCell' deletes the content of the cell in a given index of a
-- @SpreadSheet@
--
-- >>> c = Cell 0 0 (Just "1 + 2") (Right "3")
-- >>> clearCell (0,0) $ addCell c empty
-- Range (0,0) (0,0)
-- fromList [((0,0),(0, 0): Nothing Right "")]
clearCell :: Index -> SpreadSheet Cell -> SpreadSheet Cell
clearCell = addCell . emptyCell
