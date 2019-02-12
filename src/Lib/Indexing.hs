{-| This module contains functions to deal with indices and references
    in spread sheets.-}
module Lib.Indexing
  ( -- * Parsing
    parseReferences
  , referencesRegex
  , columnRegex
  , rowRegex
  , matchReferences
  , obtainRowCol
    -- * Auxiliar
  , rowColToInt
  , rowToInt
  , colToInt
  , intToRow
  , intToCol
  ) where

import Data.Cell

import Data.Char (ord, chr)
import Data.Maybe (catMaybes)
import Text.Regex
import Text.Read (readMaybe)
import GHC.Natural (intToNatural, naturalToInt)

{-| 'parseReferences' obtains all the indices from the references of an input @String@. -}
parseReferences :: String -> [Index]
parseReferences = catMaybes . map rowColToInt . catMaybes . map obtainRowCol . matchReferences

{-| The 'referencesRegex' value defines a regex to match references. -}
referencesRegex :: Regex
referencesRegex = mkRegex "[A-Z]+[0-9]+[0-9]*"

{-| The 'columnRegex' value defines a regex to match the column in a
    reference. -}
columnRegex :: Regex
columnRegex = mkRegex "[A-Z]+"

{-| The 'rowRegex' value defines a regex to match the row in a
    reference. -}
rowRegex :: Regex
rowRegex = mkRegex "[0-9]+[0-9]*"

{-| 'matchReferences' obtains all the matching references in the input. -}
matchReferences :: String -> [String]
matchReferences = maybe [] f . matchRegexAll referencesRegex
    where f (_, x, xs, _) = x : matchReferences xs

{-| 'obtainRowCol' obtains the row and the column from an input
    reference. It returns @Nothing@ if it can not match the row or the
    column. -}
obtainRowCol :: String -> Maybe (String, String)
obtainRowCol xs = do
  row <- matchRegexAll rowRegex xs
  col <- matchRegexAll columnRegex xs
  return (snd row, snd col)
  where
    snd (_, x, _, _) = x

{-| 'rowColToInt' takes a row and a column and returns an index if
    succeeds.
-}
rowColToInt :: (String, String) -> Maybe Index
rowColToInt (r, c) = do
  row <- rowToInt r
  col <- colToInt c
  return (row, col)

{-| 'rowToInt' takes a @String@ and returns a row index value if succeeds. -}
rowToInt :: String -> Maybe Row
rowToInt = readMaybe

{-| 'colToInt' takes a String and returns a column index value if
    succeeds. -}
colToInt :: String -> Maybe Col -- TODO now it does not work with AA AAA...
colToInt (x:xs) = Just $ intToNatural $ ord x - 65 -- unsafe


{-| 'intToRow' takes an index row value and returns a row reference. -}
intToRow :: Row -> String
intToRow = show

{-| 'intToCol' takes an index column and returns a column reference. -}
intToCol :: Col -> String       -- TODO fix for greater than 27
intToCol c = chr (naturalToInt $ c + (65)) : []
