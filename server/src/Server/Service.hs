{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Server.Service where

import Data.Aeson.Types

import Text.Latex
import Data.Text.Lazy hiding (map,lines)
import Logic.Proof
import Web.Scotty
import qualified Kumar.Display as Display
import qualified Kumar as K
import qualified Kumar.Operational as Operational
import qualified Parse.Kumar.Parser as KP
import qualified Hoohui
import qualified Hoohui.Parser

import Network.HTTP.Types (status500)
import Control.Exception (SomeException)
import qualified Hoohui.Parser as Hoohui


applicationServices 
  = [ parseService
    , hoohuiService
    ]


instance ToJSON (Proof String) where
  toJSON (Proof c ps) = object ["conclusion" .= c, "premises" .= ps]


-- jsonData :: FromJSON a => ActionM a
-- captureParam :: Parsable a => Text -> ActionM a

data ParseRequest = ParseRequest { source :: String, query :: String } deriving Show

instance FromJSON ParseRequest where
  parseJSON (Object v) = ParseRequest <$> v .: "source" <*> v .: "query"
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


mkParseService' :: (Latex j, Explain j, Show j) => String -> (ParseRequest -> j) -> (Proof j -> Proof j) -> ScottyM ()
mkParseService' rt mkJ format = post (capture rt) $ catch action handle
  where action = do
          req <- jsonData :: ActionM ParseRequest
          () <- liftIO $ print req
          let j = mkJ req
          () <- liftIO $ print j
          let Just tr = prove j
          () <- liftIO $ print tr
          json $ fmap latex (format tr)


mkParseService :: (Latex j, Explain j, Show j) => String -> (ParseRequest -> j) -> ScottyM ()
mkParseService rt mkJ = mkParseService' rt mkJ id

hoohuiService :: ScottyM ()
-- hoohuiService = mkParseService "/hoohui" action
--   where action (ParseRequest s q) = Hoohui.parseJudgement s q
hoohuiService = post "/hoohui" $ catch action handle
  where action = do
          req <- jsonData :: ActionM ParseRequest
          
          let q = Hoohui.Parser.parseTerm (query req)
          () <- liftIO $ putStrLn $ Hoohui.ppTerm q
          () <- liftIO $ print q

          let rss = Hoohui.Parser.parseRuleSystem (source req)
          () <- liftIO $ mapM_ putStrLn $ lines (source req)

          let j = Hoohui.parseJudgement (source req) (query req)
          -- () <- liftIO $ print (Hoohui.parseRuleSystem (source req))
          let tr = Hoohui.prove' j
          json $ fmap latex tr