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
import Web.SpiraJira.Transition

-- # pp conn.get( "/rest/api/2/issue/IB-6292" ).body
-- # pp conn.get( "/rest/api/2/issue/IB-6292/comment" ).body
-- # pp conn.post( "/rest/api/2/issue/IB-6292/comment", body: comment )
-- # pp conn.get( "/rest/api/2/issue/IB-6292/comment" ).body
-- puts conn.get( "/rest/api/2/issue/IB-6292/comment" ).body["comments"].last["body"]

-- http GET -a ttripp:xZ\?3KEwGLt3ZXFit https://localhost:8080/rest/api/2/issue/IB-6292

issueUrl       baseUrl key = baseUrl ++ "issue/" ++ (show key)
commentsUrl    baseUrl key = (issueUrl baseUrl key) ++ "/comment"
transitionsUrl baseUrl key = (issueUrl baseUrl key) ++ "/transitions"

issue :: JiraConfig -> TicketId -> IO (Either String Issue)
issue jira id = do
  body <- get jira $ issueUrl (uri jira) id
  return $ decodeIssue body

transitions config ticket = do
  let u = transitionsUrl (uri config) ticket
  body <- get config u
  return $ decodeTransitions body

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

showTransitions config ticket = do
  d <- transitions config ticket
  case d of
   Left  err -> putStrLn err
   Right i   -> putStrLn $ unlines (map show i)

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
    ("actions":tid:[]) -> showTransitions j (TicketId $ pack tid)
    _  -> putStrLn "Option not recognized"
