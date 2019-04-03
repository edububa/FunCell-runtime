{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-| This module contains the type definitions and instances to deal with
    the Cells. -}
module Data.Cell
  ( -- * Type synonyms
    Row
  , Col
  , Index
    -- * Data type
  , Cell(..)
  ) where

import Data.Aeson
import GHC.Generics
import Numeric.Natural

-- Lib data types

type Row = Natural
type Col = Natural
type Error = String
type Index = (Row, Col)

data Cell = Cell { row :: Row
                 , col :: Col
                 , content :: Maybe String
                 , evalResult :: Either Error String } deriving (Generic)

instance Show Cell where
  show (Cell r c cont res) = "(" <> show r <> ", " <> show c <> "): " <>
                             show cont <> "\n\t" <> show res

instance FromJSON Cell where
  parseJSON = withObject "cell" $ \o -> do
    row     <- o .: "row"
    col     <- o .: "col"
    content <- o .: "content"
    return $ Cell row col content (Right "")


instance ToJSON Cell
