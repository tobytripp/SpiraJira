{-# LANGUAGE OverloadedStrings #-}
module Main where


import Control.Applicative
import Control.Exception
import Data.Text (unpack, pack)
import Options
import qualified Data.ByteString.Char8 as BS

import Web.SpiraJira.Issue
import Web.SpiraJira.Config
import Web.SpiraJira.Connection

-- # pp conn.get( "/rest/api/2/issue/IB-6292" ).body
-- # pp conn.get( "/rest/api/2/issue/IB-6292/comment" ).body
-- # pp conn.post( "/rest/api/2/issue/IB-6292/comment", body: comment )
-- # pp conn.get( "/rest/api/2/issue/IB-6292/comment" ).body
-- puts conn.get( "/rest/api/2/issue/IB-6292/comment" ).body["comments"].last["body"]

-- http GET -a ttripp:xZ\?3KEwGLt3ZXFit https://localhost:8080/rest/api/2/issue/IB-6292

issueUrl    baseUrl key = baseUrl ++ "issue/" ++ (show key)
commentsUrl baseUrl key = (issueUrl baseUrl key) ++ "/comment"

issue :: JiraConfig -> TicketId -> IO (Either String Issue)
issue jira id = do
  body <- get jira $ issueUrl (uri jira) id
  return $ decodeIssue body

showIssue :: JiraConfig -> TicketId -> IO ()
showIssue j tid = do
  d <- issue j tid
  case d of
   Left  err -> putStrLn err
   Right i   -> putStrLn $ show i

postComment :: JiraConfig -> TicketId -> String -> IO ()
postComment config ticket comment = do
  let u = commentsUrl (uri config) ticket
  putStrLn u
  post config u $ commentJson comment

data MainOptions = MainOptions {configPath :: String}

instance Options MainOptions where
  defineOptions = pure MainOptions
    <*> simpleOption "config" "spirajira.cfg" "Configuration path."

main :: IO ()
main = runCommand $ \ options args -> do
  j <- parseConfig (configPath options)
  case args of
    [] -> putStrLn "Please provide a ticket ID"
    (tid : []) -> showIssue j (TicketId $ pack tid)
    ("comment":tid:comment:[]) -> postComment j (TicketId $ pack tid) comment
    _  -> putStrLn "Option not recognized"
