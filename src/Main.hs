{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.HTTP.Conduit
import qualified Network.Connection as Conn
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import Data.Text (unpack)

import Issue

-- # pp conn.get( "/rest/api/2/issue/IB-6292" ).body
-- # pp conn.get( "/rest/api/2/issue/IB-6292/comment" ).body
-- # pp conn.post( "/rest/api/2/issue/IB-6292/comment", body: comment )
-- # pp conn.get( "/rest/api/2/issue/IB-6292/comment" ).body
-- puts conn.get( "/rest/api/2/issue/IB-6292/comment" ).body["comments"].last["body"]

-- http GET -a ttripp:xZ?3KEwGLt3ZXFit https://localhost:8080/rest/api/2/issue/IB-6292

user = BS.pack "ttripp"
pass = BS.pack ""

baseUrl = "https://localhost:8080/rest/api/2/"
issueUrl key    = baseUrl ++ "issue/" ++ key
commentsUrl key = (issueUrl key) ++ "comment"

-- http://stackoverflow.com/questions/21310690/disable-ssl-tls-certificate-validation-in-network-http-conduit
nonVerifyingManager :: IO Manager
nonVerifyingManager =
  let tlsSettings = Conn.TLSSettingsSimple {
        Conn.settingDisableCertificateValidation = True,
        Conn.settingDisableSession               = False,
        Conn.settingUseServerName                = True}
  in newManager $ mkManagerSettings tlsSettings Nothing

req :: String -> Request
req uri = applyBasicAuth user pass $ fromJust $ parseUrl uri

https :: Manager -> String -> IO LBS.ByteString
https manager url = fmap responseBody $ httpLbs (req url) manager

issue key = do
  manager <- nonVerifyingManager
  body    <- https manager $ issueUrl key
  return $ decodeIssue body

main :: IO ()
main = do
  d <- issue "IB-6292"
  case d of
   Left  err -> putStrLn err
   Right i -> putStrLn $ unpack $ issueDescription i
  putStrLn "done!"
