{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Server.Service where

import Data.Aeson.Types

import Latex
import qualified Parser
import Data.Text.Lazy hiding (map)
import Logic.Proof
import Web.Scotty
import qualified Display
import qualified Kumar as K
import qualified Operational

import Network.HTTP.Types (status500)
import Control.Exception (SomeException)



instance ToJSON (Proof String) where
  toJSON (Proof c ps) = object ["conclusion" .= c, "premises" .= ps]


-- jsonData :: FromJSON a => ActionM a
-- captureParam :: Parsable a => Text -> ActionM a

data ParseRequest = ParseRequest { source :: String } deriving Show

instance FromJSON ParseRequest where
  parseJSON (Object v) = ParseRequest <$> v .: "source"
  parseJSON _ = fail "Expected an object"

parseService :: ScottyM ()
parseService = post "/parse" $ catch action handle
  where action = do
          req <- jsonData :: ActionM ParseRequest
          let e = source req
          liftIO $ print e
          case K.parseExprSafe e of 
            Right e' -> do
              let ej = Operational.infer [] e'
              liftIO $ print ej
            Left e' -> do
              () <- liftIO $ print e'
              return ()
          let tr = Display.pt' e -- pt' (Parser.parse e)
          -- liftIO $ print tr
          json $ fmap latex tr
        handle e = do
          status status500
          liftIO $ print (e :: SomeException)
          json $ object ["error" .= pack (show e)]

