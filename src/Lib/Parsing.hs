{-| This module contains functions to deal with parsing of a cell
  content.-}
module Lib.Parsing
  ( -- * Reference Functions
    parseReferences
    -- ** Auxiliar Reference Functions
  , matchReferences
    -- ** Desugar Functions
  , desugarContent
  , desugarRange
  , desugarList
    -- ** Auxiliar Desugar Functions
  , desugar
  , genRange
  , rangeToIndex
  , toListString
  , toRangeString
  , showRefs
  ) where

-- external imports
import Data.String.Utils (replace)
import Text.Regex
import Data.Maybe (catMaybes)
-- internal imports
import Data.Cell
import Data.Parsing
import Lib.Indexing

{-| 'parseReferences' obtains all the indices from the references of an
  input @String@. -}
parseReferences :: String -> [Index]
parseReferences = catMaybes . map rowColToInt . catMaybes . map obtainRowCol . matchReferences

{-| 'matchReferences' obtains all the matching references in the input. -}
matchReferences :: String -> [String]
matchReferences = maybe [] f . matchRegexAll referencesRegex
    where f (_, x, xs, _) = x : matchReferences xs

{-| 'desugarContent' parses the input, checks that indices are correct
  and desugar them and changes the ranges of values. An example:

>>> desugarContent "(A0:B2)"
Right "(A0 A1 A2 B0 B1 B2)"
>>> desugarContent "(B2:A0)"
Left "Incorrect index" -}
desugarContent :: String -> Either Error String
desugarContent s = do
  res  <- desugarRange s
  res' <- desugarList res
  return res'

{-| 'desugarRange' is a specialized version of the 'desugar' function
  to deal with ranges.

>>> desugarRange "(A1:B2)"
Right "(A1 A2 B1 B2)"
>>> desugarRange "[A1:B2]"
Right "[A1:B2]"
>>> desugarRange "(A1:B0)"
Left "Incorrect index" -}
desugarRange :: String -> Either Error String
desugarRange = desugar rangeRegex toRangeString

{-| 'desugarList' is a specialized version of the 'desugar' function to
  deal with lists.

>>> desugarList "[A0:B3]"
Right "[A0,A1,A2,A3,B0,B1,B2,B3]"
>>> desugarList "[B3:A0]"
Left "Incorrect index" -}
desugarList :: String -> Either Error String
desugarList = desugar listRegex toListString

{-| 'desugar' is a higher order function that matches a regex with the
  input string generates the ranges and finally returns the
  desugarized string. -}
desugar :: Regex -> (String -> [Index] -> String -> String)
        -> String -> Either Error String
desugar regex f s | res  == Nothing = Right s
                  | otherwise = do
                      let Just (pre, matched, post, _) = res
                      is <- rangeToIndex matched
                      rs <- genRange (fst is) (snd is)
                      return $ f pre rs post
                        where res = matchRegexAll regex s

{-| 'genRange' returns the range between two given indices.

>>> genRange (0,0) (2,2)
Right [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)]
>>> genRange (2,2) (0,0)
Left "Incorrect index" -}
genRange :: Index -> Index -> Either Error [Index]
genRange (x1, y1) (x2, y2) | x1 <= x2 && y1 <= y2 =
                               Right $ (,) <$> [y1..y2] <*> [x1..x2]
                           | otherwise = Left "Incorrect index"

{-| 'rangeToIndex' translates a range to its indices.

>>> rangeToIndex "(A1:B2)"
Right ((1,0),(2,1))
>>> rangeToIndex "(A1:)"
Left "Cannot parse index" -}
rangeToIndex :: String -> Either Error (Index, Index)
rangeToIndex = toTuple . parseReferences
  where toTuple (x:y:[]) = Right (x, y)
        toTuple _        = Left "Cannot parse index"

{-| 'toListString' -}
toListString :: String -> [Index] -> String -> String
toListString pre xs post = pre <> "[" <> (replace " " "," $ showRefs xs) <> "]"<> post

{-| 'toRangeString' -}
toRangeString :: String -> [Index] -> String -> String
toRangeString pre xs post = pre <> "(" <> showRefs xs <> ")" <> post

{-| 'showRefs' -}
showRefs :: [Index] -> String
showRefs = unwords . map indexToRef
