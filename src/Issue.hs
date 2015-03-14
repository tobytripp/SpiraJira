{-# LANGUAGE OverloadedStrings #-}
module Issue (
  Issue(..),
  issueDescription,
  decodeIssue
  ) where

import Data.Aeson(Value(..), (.:), FromJSON(..), eitherDecode)
import Control.Monad
import Control.Applicative
import Data.Text
import qualified Data.ByteString.Lazy as LBS


data Fields = Fields {
  summary     :: Text,
  description :: Text
  } deriving (Eq, Show)

data Issue = Issue {
  key    :: Text,
  fields :: Fields
  } deriving (Eq, Show)

issueDescription :: Issue -> Text
issueDescription issue = description $ fields issue

instance FromJSON Fields where
  parseJSON (Object v) =
    Fields <$> v .: "summary"
           <*> v .: "description"
  parseJSON _ = mzero

instance FromJSON Issue where
  parseJSON (Object v) = do
    key <- v .: "key"
    fields <-  parseJSON =<< (v .: "fields")
    return $ Issue key fields
  parseJSON _ = mzero

decodeIssue :: LBS.ByteString -> Either String Issue
decodeIssue json = eitherDecode json

