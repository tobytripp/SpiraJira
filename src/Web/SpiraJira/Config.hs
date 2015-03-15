{-# LANGUAGE OverloadedStrings #-}
module Web.SpiraJira.Config (
  JiraConfig(..),
  parseConfig
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.Configurator     as Config
import Data.Configurator.Types (Config, Name)

data JiraConfig = JiraConfig {
  username :: BS.ByteString,
  password :: BS.ByteString,
  uri      :: String
  } deriving (Eq, Show)

parseConfig :: FilePath -> IO JiraConfig
parseConfig path = do
  c <- Config.load [
    Config.Optional "$(HOME)/.spirajira",
    Config.Optional "../spirajira.cfg",
    Config.Optional path]
  u <- Config.lookupDefault "Missing User"     c "user"
  p <- Config.lookupDefault "Missing Password" c "pass"
  l <- Config.lookupDefault "Missing URI"      c "uri"
  return $ JiraConfig (BS.pack u) (BS.pack p) l
