{-# LANGUAGE OverloadedStrings #-}
module Main where


import Control.Applicative
import Control.Exception
import Data.Configurator.Types (Config, Name)
import Data.Maybe
import Data.Text (unpack)
import Network.HTTP.Conduit
import Options
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Configurator as Config
import qualified Network.Connection as Conn

import Issue

-- # pp conn.get( "/rest/api/2/issue/IB-6292" ).body
-- # pp conn.get( "/rest/api/2/issue/IB-6292/comment" ).body
-- # pp conn.post( "/rest/api/2/issue/IB-6292/comment", body: comment )
-- # pp conn.get( "/rest/api/2/issue/IB-6292/comment" ).body
-- puts conn.get( "/rest/api/2/issue/IB-6292/comment" ).body["comments"].last["body"]

-- http GET -a ttripp:xZ?3KEwGLt3ZXFit https://localhost:8080/rest/api/2/issue/IB-6292

data JiraConfig = JiraConfig {
  username :: BS.ByteString,
  password :: BS.ByteString,
  uri  :: String
  } deriving (Eq, Show)

parseConfig :: FilePath -> IO JiraConfig
parseConfig path = do
  c <- Config.load [Config.Required path]
  u <- Config.lookupDefault "Missing User"     c "user"
  p <- Config.lookupDefault "Missing Password" c "pass"
  l <- Config.lookupDefault "Missing URI"      c "uri"
  return $ JiraConfig (BS.pack u) (BS.pack p) l

issueUrl    baseUrl key = baseUrl ++ "issue/" ++ (show key)
commentsUrl baseUrl key = (issueUrl baseUrl key) ++ "comment"

-- http://stackoverflow.com/questions/21310690/disable-ssl-tls-certificate-validation-in-network-http-conduit
nonVerifyingManager :: IO Manager
nonVerifyingManager =
  let tlsSettings = Conn.TLSSettingsSimple {
        Conn.settingDisableCertificateValidation = True,
        Conn.settingDisableSession               = False,
        Conn.settingUseServerName                = True}
  in newManager $ mkManagerSettings tlsSettings Nothing

req :: JiraConfig -> String -> Request
req j uri = applyBasicAuth (username j) (password j) $ fromJust $ parseUrl uri

https :: Manager -> JiraConfig -> String -> IO LBS.ByteString
https manager jira url = fmap responseBody $ httpLbs (req jira url) manager

issue :: JiraConfig -> TicketId -> IO (Either String Issue)
issue jira key = do
  manager <- nonVerifyingManager
  body    <- https manager jira $ issueUrl (uri jira) key
  return $ decodeIssue body

data MainOptions = MainOptions {
  configPath :: String}

instance Options MainOptions where
  defineOptions = pure MainOptions
    <*> simpleOption "config" "../spirajira.cfg" "Configuration path."

main :: IO ()
main = runCommand $ \options args -> do
  putStrLn $ configPath options
  putStrLn $ show args

  j <- parseConfig (configPath options)
  d <- issue j (TicketId "IB-6292")
  case d of
   Left  err -> putStrLn err
   Right i   -> putStrLn $ show $ comments i
  putStrLn "done!"
