{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Data.Aeson
import Data.Text
import GHC.Generics

data Cell = Cell { row :: Int
                 , col :: Int
                 , content :: Maybe String
                 , evalResult :: Either String String } deriving (Show, Generic)

instance FromJSON Cell where
  parseJSON = withObject "cell" $ \o -> do
    row     <- o .: "row"
    col     <- o .: "col"
    content <- o .: "content"
    return $ Cell row col content (Right "")


instance ToJSON Cell
