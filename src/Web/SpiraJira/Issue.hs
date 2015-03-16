{-# LANGUAGE OverloadedStrings #-}
module Web.SpiraJira.Issue (
  Issue,
  Comment,
  TicketId(..),
  issueDescription,
  decodeIssue,
  commentJson
  ) where

import Data.Aeson(Value(..), (.:), (.=), FromJSON(..), ToJSON(..), eitherDecode, encode, object)
import Data.Maybe
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format
import Control.Monad
import Control.Applicative
import Data.Text (Text (..), unpack, pack)
import qualified Data.ByteString.Lazy as LBS
import System.Locale (defaultTimeLocale)

newtype TicketId = TicketId Text deriving (Eq)
instance Show TicketId where
  show (TicketId key) = unpack key

newtype JiraTime = JiraTime {utc :: UTCTime} deriving (Eq)
instance Show JiraTime where
  show (JiraTime utc) = show utc

jiraFormat :: String
jiraFormat = "%FT%T%Q%z"

empty_time = JiraTime $ fromJust $ parseTime defaultTimeLocale "%FT%T" "1970-01-01T00:00:00"

instance FromJSON JiraTime where
  parseJSON (String s) = maybe mzero (return . JiraTime) $
                         parseTime defaultTimeLocale jiraFormat (unpack s)
  parseJSON _ = mzero

data Fields = Fields {
  summary     :: Text,
  description :: Text,
  status      :: Text
  } deriving (Eq)

instance Show Fields where
  show (Fields summary description status) =
    (unpack summary) ++ "\n" ++
    "[" ++ (unpack status)  ++ "]\n\n" ++
    (unpack description) ++ "\n"

data Author = Author {
  name :: Text,
  displayName :: Text,
  email :: Text
  } deriving (Eq, Show)
empty_author = Author "" "" ""

data Comment = Comment {
  body    :: Text,
  author  :: Author,
  updated :: JiraTime,
  created :: JiraTime
  } deriving (Eq)

instance Show Comment where
  show (Comment body author updated _) =
    (unpack $ displayName author) ++
    " (" ++ (show updated) ++ ")\n" ++
    (unpack body) ++ "\n"

data Issue = Issue {
  key      :: TicketId,
  fields   :: Fields,
  comments :: [Comment]
  } deriving (Eq)

instance Show Issue where
  show (Issue key fields comments) =
    show key    ++ " :: " ++
    show fields ++
    "\n\nCOMMENTS:\n\n" ++
    unlines (map show comments)  ++ "\n"

issueDescription :: Issue -> Text
issueDescription issue = description $ fields issue

commentJson :: String -> LBS.ByteString
commentJson body = encode $ Comment (pack body) empty_author empty_time empty_time

instance FromJSON Fields where
  parseJSON (Object v) =
    Fields <$> v .: "summary"
           <*> v .: "description"
           <*> ((v .: "status") >>= (.: "name"))
  parseJSON _ = mzero

instance FromJSON Author where
  parseJSON (Object v) =
    Author <$> v .: "name"
           <*> v .: "displayName"
           <*> v .: "emailAddress"
  parseJSON _ = mzero

instance ToJSON Comment where
  toJSON (Comment body _ _ _) = object ["body" .= body]

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

