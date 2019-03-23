{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.ExternalModule where

import Data.Aeson
import Data.Text
import GHC.Generics

data ExternalModule = ExternalModule String deriving (Show, Generic)

instance FromJSON ExternalModule where
  parseJSON = withObject "externalModule" $ \o -> do
    text <- o .: "text"
    return $ ExternalModule text
