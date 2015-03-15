{-# LANGUAGE OverloadedStrings #-}
module Web.SpiraJira.Connection (
  get,
  post
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as LBS
import Data.Maybe
import Network.HTTP.Conduit
import qualified Network.Connection    as Conn

import Web.SpiraJira.Config

-- http://stackoverflow.com/questions/21310690/disable-ssl-tls-certificate-validation-in-network-http-conduit
nonVerifyingManager :: IO Manager
nonVerifyingManager =
  let tlsSettings = Conn.TLSSettingsSimple {
        Conn.settingDisableCertificateValidation = True,
        Conn.settingDisableSession               = False,
        Conn.settingUseServerName                = True}
  in newManager $ mkManagerSettings tlsSettings Nothing

authorizedRequest :: JiraConfig -> String -> Request
authorizedRequest j uri = applyBasicAuth (username j) (password j) $ fromJust $ parseUrl uri

https :: Manager -> JiraConfig -> String -> IO LBS.ByteString
https manager jira url =
  fmap responseBody $ httpLbs (authorizedRequest jira url) manager

get :: JiraConfig -> String -> IO LBS.ByteString
get jira uri = do
  manager <- nonVerifyingManager
  body    <- https manager jira uri
  return body

post :: JiraConfig -> String -> LBS.ByteString -> IO ()
post config uri body = do
  manager <- nonVerifyingManager
  let request = authorizedRequest config uri
  let postReq = request {
        requestBody    = RequestBodyLBS body,
        requestHeaders = ("Content-Type", "application/json") : (requestHeaders request),
        method         = "POST" }
  putStrLn $ show postReq
  response <- httpLbs postReq manager
  liftIO $ do
    print $ responseStatus response
    mapM_ print $ responseHeaders response


