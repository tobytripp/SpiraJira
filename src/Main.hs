{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Data.Text (pack)
import Options

import Web.SpiraJira.Issue
import Web.SpiraJira.Config
import Web.SpiraJira.Connection
import Web.SpiraJira.Transition

-- http GET -a ttripp:xZ\?3KEwGLt3ZXFit https://localhost:8080/rest/api/2/issue/IB-6292

issueUrl :: String -> TicketId -> String
issueUrl baseUrl key = baseUrl ++ "issue/" ++ (show key)

commentsUrl :: String -> TicketId -> String
commentsUrl baseUrl key = (issueUrl baseUrl key) ++ "/comment"

transitionsUrl :: String -> TicketId -> String
transitionsUrl baseUrl key = (issueUrl baseUrl key) ++ "/transitions"

issue :: JiraConfig -> TicketId -> IO (Either String Issue)
issue jira ticket = do
  body <- get jira $ issueUrl (uri jira) ticket
  return $ decodeIssue body

transitions :: JiraConfig -> TicketId -> IO (Either String [Transition])
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
  post config u $ commentJson comment

showTransitions :: JiraConfig -> TicketId -> IO ()
showTransitions config ticket = do
  d <- transitions config ticket
  case d of
   Left  err -> putStrLn err
   Right i   -> putStrLn $ unlines (map show i)

setTransition :: JiraConfig -> TicketId -> String -> IO ()
setTransition config ticket stateId = do
  let u = transitionsUrl (uri config) ticket
  post config u $ transitionJson stateId

data MainOptions = MainOptions {configPath :: String}

instance Options MainOptions where
  defineOptions = pure MainOptions
    <*> simpleOption "config" "spirajira.cfg" "Configuration path."

main :: IO ()
main = runCommand $ \ options args -> do
  j <- parseConfig (configPath options)
  case args of
    []                         -> putStrLn "Please provide a ticket ID"
    (tid : [])                 -> showIssue       j (TicketId $ pack tid)
    ("comment":tid:comment:[]) -> postComment     j (TicketId $ pack tid) comment
    ("actions":tid:[])         -> showTransitions j (TicketId $ pack tid)
    ("act":ticket:transitionId:[])  ->
      setTransition j (TicketId $ pack ticket) transitionId
    _                          -> putStrLn "Option not recognized"
