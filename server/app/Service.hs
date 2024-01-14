{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Service where

import Data.Aeson.Types

import Syntax
import qualified Parser
import Data.Text.Lazy hiding (map)
import Logic.Proof
import Web.Scotty



instance ToJSON (Proof String) where
  toJSON (Proof c ps) = object ["conclusion" .= c, "premises" .= ps]


-- jsonData :: FromJSON a => ActionM a
-- captureParam :: Parsable a => Text -> ActionM a

data ParseRequest = ParseRequest { source :: String } deriving Show

instance FromJSON ParseRequest where
  parseJSON (Object v) = ParseRequest <$> v .: "source"
  parseJSON _ = fail "Expected an object"

parseService :: ScottyM ()
parseService = post "/parse" $ do

    req <- jsonData :: ActionM ParseRequest
    let e = source req
    liftIO $ print e
    let tr = pt' (Parser.parse e)
    liftIO $ print tr
    json $ fmap latex tr
    -- addHeader "Access-Control-Allow-Origin" "*"
    -- addHeader "Access-Control-Allow-Headers" "Content-Type"
    -- addHeader "Access-Control-Allow-Methods" "POST, GET, OPTIONS"
    -- addHeader "Access-Control-Allow-Credentials" "true"

    -- beam <- captureParam "source"
    -- let e = Parser.parse beam
    -- let tr = pt' e
    -- let jsn = toJson tr
    -- json $ pack (jsn)

