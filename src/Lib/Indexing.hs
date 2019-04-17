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

-- external imports
import Data.Char (ord, chr)
import Data.Maybe (catMaybes)
import Text.Regex
import Text.Read (readMaybe)
import GHC.Natural (intToNatural, naturalToInt)
-- internal imports
import Data.Cell

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
  r <- matchRegexAll rowRegex xs
  c <- matchRegexAll columnRegex xs
  return (snd' r, snd' c)
  where
    snd' (_, x, _, _) = x

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
