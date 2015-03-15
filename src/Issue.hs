{-# LANGUAGE OverloadedStrings #-}
module Issue (
  Issue(..),
  TicketId(..),
  issueDescription,
  decodeIssue
  ) where

import Data.Aeson(Value(..), (.:), FromJSON(..), eitherDecode)
import Data.Time.Clock (UTCTime)
import Data.Time.Format
import Control.Monad
import Control.Applicative
import Data.Text
import qualified Data.ByteString.Lazy as LBS
import System.Locale (defaultTimeLocale)

newtype TicketId = TicketId Text deriving (Eq)
instance Show TicketId where
  show (TicketId key) = unpack key

newtype JiraTime = JiraTime {utc :: UTCTime} deriving (Eq, Show)
jiraFormat :: String
jiraFormat = "%FT%T%Q%z"

instance FromJSON JiraTime where
  parseJSON (String s) = maybe mzero (return . JiraTime) $
                         parseTime defaultTimeLocale jiraFormat (unpack s)
  parseJSON _ = mzero

data Fields = Fields {
  summary     :: Text,
  description :: Text
  } deriving (Eq, Show)

data Author = Author {
  name :: Text,
  displayName :: Text,
  email :: Text
  } deriving (Eq, Show)

data Comment = Comment {
  body    :: Text,
  author  :: Author,
  updated :: JiraTime,
  created :: JiraTime
  } deriving (Eq, Show)

data Issue = Issue {
  key      :: TicketId,
  fields   :: Fields,
  comments :: [Comment]
  } deriving (Eq, Show)

issueDescription :: Issue -> Text
issueDescription issue = description $ fields issue

instance FromJSON Fields where
  parseJSON (Object v) =
    Fields <$> v .: "summary"
           <*> v .: "description"
  parseJSON _ = mzero

instance FromJSON Author where
  parseJSON (Object v) =
    Author <$> v .: "name"
           <*> v .: "displayName"
           <*> v .: "emailAddress"
  parseJSON _ = mzero

instance FromJSON Comment where
  parseJSON (Object v) = do
    body <-  v .: "body"
    author <- parseJSON =<< (v .: "author")
    updated <- v .: "updated"
    created <- v .: "created"
    return $ Comment body author updated created
  parseJSON _ = mzero

instance FromJSON Issue where
  parseJSON (Object v) = do
    key    <- v .: "key"
    fields <-  parseJSON =<< (v .: "fields")
    comments <- mapM parseJSON =<< ((v .: "fields") >>= (.: "comment") >>= (.: "comments"))
    return $ Issue (TicketId key) fields comments
  parseJSON _ = mzero

decodeIssue :: LBS.ByteString -> Either String Issue
decodeIssue json = eitherDecode json
