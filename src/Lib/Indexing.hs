module Lib.Indexing where

import Data.Cell

import Data.Char (ord, chr)
import Data.Maybe (catMaybes)
import Text.Regex
import Text.Read (readMaybe)

parseIndices :: String -> [(Col, Row)]
parseIndices = catMaybes . map rowColToInt . catMaybes . map obtainRowCol . obtainIndices

indexRegex :: Regex
indexRegex = mkRegex "[A-Z]+[0-9]+[0-9]*"

columnRegex :: Regex
columnRegex = mkRegex "[A-Z]+"

rowRegex :: Regex
rowRegex = mkRegex "[0-9]+[0-9]*"

obtainIndices :: String -> [String]
obtainIndices = maybe [] f . matchRegexAll indexRegex
    where f (_, x, xs, _) = x : obtainIndices xs

obtainRowCol :: String -> Maybe (String, String)
obtainRowCol xs = do
  row <- matchRegexAll rowRegex xs
  col <- matchRegexAll columnRegex xs
  return (snd row, snd col)
  where
    snd (_, x, _, _) = x

rowColToInt :: (String, String) -> Maybe (Row, Col)
rowColToInt (r, c) = do
  row <- rowToInt r
  col <- colToInt c
  return (row, col)

rowToInt :: String -> Maybe Int
rowToInt = readMaybe

colToInt :: String -> Maybe Int -- TODO now it does not work with AA AAA...
colToInt (x:xs) = Just $ ord x - 65

intToRow :: Int -> String
intToRow = show

intToCol :: Int -> String       -- TODO fix for greater than 27
intToCol c = chr (c + 65) : []
