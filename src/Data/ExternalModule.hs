{-| This module contains the definition of the ExternalModule data type
  and some useful instances. -}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.ExternalModule where

-- external imports
import Data.Aeson
import GHC.Generics

data ExternalModule = ExternalModule String deriving (Show, Generic)

instance FromJSON ExternalModule where
  parseJSON = withObject "externalModule" $ \o -> do
    text <- o .: "text"
    return $ ExternalModule text

instance ToJSON ExternalModule where
  toJSON (ExternalModule e) =
    object [ "text" .= e ]
