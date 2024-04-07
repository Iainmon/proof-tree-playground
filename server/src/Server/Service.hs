{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Server.Service where

import Data.Aeson.Types

import Text.Latex
import Data.Text.Lazy hiding (map)
import Logic.Proof
import Web.Scotty
import qualified Display
import qualified Kumar as K
import qualified Operational
import qualified Parse.Kumar.Parser as KP

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


handle e = do
  status status500
  liftIO $ putStrLn ("error message: " ++ show (e :: SomeException))
  json $ object ["error" .= pack (show e)]


parseService :: ScottyM ()
parseService = post "/parse" $ catch action handle
  where action = do
          req <- jsonData :: ActionM ParseRequest
          let e = source req
          liftIO $ putStrLn ("[input]\t " ++ show e)
          liftIO $ putStrLn ("[tokens]\t " ++ show (KP.tokenize e))
          case K.parseExprSafe e of 
            Right e' -> do
              let ep = KP.parseExpr e
              liftIO $ putStrLn ("[parsed]\t " ++ show ep)
              liftIO $ putStrLn ""
              liftIO $ putStrLn ("[expr]\t " ++ show e')
              liftIO $ putStrLn ""
              let ej = Operational.infer [] e'
              liftIO $ putStrLn ("[query]\t " ++ show ej)
            Left e' -> do
              () <- liftIO $ putStrLn ("[error]\t " ++ show e')
              return ()
          let tr = Display.pt' e -- pt' (Parser.parse e)
          -- liftIO $ print tr
          json $ fmap latex tr

