{-| -}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Messages where

-- external imports
import Data.Aeson
import GHC.Generics

data Save = Save String deriving (Show, Generic)
data Load = Load String deriving (Show, Generic)

instance FromJSON Save where
  parseJSON = withObject "message"  $ \o -> do
    path <- o .: "save"
    return $ Save path

instance FromJSON Load where
  parseJSON = withObject "message" $ \o -> do
    path <- o .: "load"
    return $ Load path
