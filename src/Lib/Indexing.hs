{-| This module contains functions to deal with indices and references
    in spread sheets.-}
module Lib.Indexing where

-- external imports
import Data.Char (ord, chr)
import Text.Read (readMaybe)
import Text.Regex (matchRegexAll)
import GHC.Natural (intToNatural, naturalToInt)
-- internal imports
import Data.Cell
import Data.Parsing

{-| 'rowColToInt' takes a row and a column and returns an index if
    succeeds.
-}
rowColToInt :: (String, String) -> Maybe Index
rowColToInt (r, c) = do
  r' <- rowToInt r
  c' <- colToInt c
  return (r', c')

{-| 'rowToInt' takes a @String@ and returns a row index value if succeeds. -}
rowToInt :: String -> Maybe Row
rowToInt = readMaybe

{-| 'colToInt' takes a String and returns a column index value if
    succeeds. -}
colToInt :: String -> Maybe Col -- TODO now it does not work with AA AAA...
colToInt (x:_) = Just $ intToNatural $ ord x - 65 -- unsafe
colToInt _ = Nothing

{-| 'intToRow' takes an index row value and returns a row reference. -}
intToRow :: Row -> String
intToRow = show

{-| 'intToCol' takes an index column and returns a column reference. -}
intToCol :: Col -> String       -- TODO fix for greater than 27
intToCol c = chr (naturalToInt $ c + (65)) : []

{-| -}
indexToRef :: Index -> String
indexToRef (x, y) = intToCol x <> intToRow y

{-| 'getIndex' returns the index of a cell -}
getIndex :: Cell -> Index
getIndex = (,) <$> row <*> col

{-| 'obtainRowCol' obtains the row and the column from an input
    reference. It returns @Nothing@ if it can not match the row or the
    column. -}
obtainRowCol :: String -> Maybe (String, String)
obtainRowCol xs = do
  r <- matchRegexAll rowRegex xs
  c <- matchRegexAll columnRegex xs
  return (snd' r, snd' c)
  where
    snd' (_, x, _, _) = x
