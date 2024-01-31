{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Service where

import Data.Aeson.Types

import Syntax
import Latex
import qualified Parser
import Data.Text.Lazy hiding (map)
import Logic.Proof
import Web.Scotty
import qualified Display



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
    let tr = Display.pt' e -- pt' (Parser.parse e)
    liftIO $ print tr
    json $ fmap latex tr
