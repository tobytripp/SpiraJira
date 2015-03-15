{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Web.SpiraJira.Transition (
  decodeTransitions
  ) where

import Data.Aeson(Value(..), (.:), (.=), FromJSON(..), ToJSON(..), eitherDecode, encode, object)
import Control.Monad
import Control.Applicative
import Data.Text (Text (..), unpack, pack)
import qualified Data.ByteString.Lazy as LBS

data Transition = Transition {
  name :: Text,
  ident :: String } deriving (Eq)

instance Show Transition where
  show (Transition name ident) =
    ident ++ " : " ++ (unpack name)

instance FromJSON Transition where
  parseJSON (Object v) =
    Transition <$> v .: "name"
               <*> v .: "id"
  parseJSON _ = mzero

instance FromJSON [Transition] where
  parseJSON (Object v) = do
    transitions <- mapM parseJSON =<< (v .: "transitions")
    return $ transitions
  parseJSON _ = mzero

decodeTransitions :: LBS.ByteString -> Either String [Transition]
decodeTransitions json = eitherDecode json
