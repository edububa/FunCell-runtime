{-| This module contains the type definitions and instances to deal with
  Cells. -}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Cell
  ( -- * Type synonyms
    Row
  , Col
  , Index
  , Error
    -- * Data type
  , Cell(..)
  ) where

-- external imports
import Data.Aeson
import Data.SpreadSheet
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
                 , evalResult :: Either Error String } deriving (Generic, Read)

instance Show Cell where
  show (Cell r c cont res) = "(" <> show r <> ", " <> show c <> "): " <>
                             show cont <> " " <> show res

instance FromJSON Cell where
  parseJSON = withObject "cell" $ \o -> do
    r  <- o .: "row"
    c  <- o .: "col"
    ct <- o .: "content"
    return $ Cell r c ct (Right "")

instance ToJSON Cell

deriving instance Generic (SpreadSheet a)

instance ToJSON a => ToJSON (SpreadSheet a)

instance FromJSON a => FromJSON (SpreadSheet a)
