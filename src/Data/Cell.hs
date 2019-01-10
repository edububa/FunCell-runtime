{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-| This module contains the type definitions and instances to deal with
    the Cells. -}
module Data.Cell
  ( -- * Type synonyms
    Row
  , Col
  , SpreadSheet
  , Index
    -- * Data type
  , Cell(..)
  ) where

import Data.Aeson
import Data.Text
import Data.Char (ord)
import GHC.Generics
import Data.Map

-- Lib data types

type Row = Int
type Col = Int
type Index = (Row, Col)
type SpreadSheet a = Map Index a

data Cell = Cell { row :: Row
                 , col :: Col
                 , content :: Maybe String
                 , evalResult :: Either String String } deriving (Show, Generic)

instance FromJSON Cell where
  parseJSON = withObject "cell" $ \o -> do
    row     <- o .: "row"
    col     <- o .: "col"
    content <- o .: "content"
    return $ Cell row col content (Right "")


instance ToJSON Cell
