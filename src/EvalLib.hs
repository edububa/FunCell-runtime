module EvalLib where

import Data.Either (partitionEithers, rights)
import Data.Either.Combinators (mapLeft)
import Data.Char (ord, chr)
import Data.String.Utils (replace)
-- import Data.List.Utils (replace)
import Data.Maybe (catMaybes)
import Language.Haskell.Interpreter as I
import Text.ParserCombinators.ReadP
import Text.Regex
import Text.Read (readMaybe)

import Data.Cell
import Data.Cell.Lib

type Error = String

parseAndEval "" _ = return $ Right ""
parseAndEval input state = do
  let (Right res) = solveDependencies input state -- dangerous!!
  putStrLn res
  res' <- I.runInterpreter $ do { setImports ["Prelude"]; eval res }
  return $ mapLeft show res'

solveDependencies :: String -> SpreadSheet Cell -> Either Error String
solveDependencies xs state =
  case parseIndices xs of
    [] -> Right xs
    indices -> Right $ foldr applyValues xs . rights .
               fmap cellToIndexAndVal . catMaybes .
               fmap (getCell' state) $ indices


applyValues :: (String, String) -> String -> String
applyValues (from, to) = replace from to

cellToIndexAndVal :: Cell -> Either Error (String, String)
cellToIndexAndVal cell = do
  case evalResult cell of
    Left  x -> Left $ "Error in cell " <> index
    Right x -> Right (index, x)
  where index = intToCol (col cell) <> intToRow (row cell)
  -- (intToCol (col cell) <> intToRow (row cell),
  -- evalResult cell)

-- Index parsing
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
