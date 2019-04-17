module Lib.CodeGen where

import Data.Cell

genSpreadSheet :: String
genSpreadSheet = "empty"

addCellCode :: Cell -> String
addCellCode cell = " & putCell " <> "(" <> r <> "," <> c <> ") " <> "(const $ " <> ct <> ")"
  where r = show $ row cell
        c = show $ col cell
        ct = maybe "" id $ content cell

genSpreadSheetAndFillWithCells :: [Cell] -> String
genSpreadSheetAndFillWithCells cs = genSpreadSheet <> foldr (mappend . addCellCode) "" cs
