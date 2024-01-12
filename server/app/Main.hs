{-# LANGUAGE OverloadedStrings #-}

module Main where
import Parser
import Data.Text.Lazy
import Logic.Proof
import Syntax
import Service

import Web.Scotty
import Network.Wai.Middleware.Cors

-- main :: IO ()
-- main = scotty 3000 $
--   get "/parse/:source" $ do
--     beam <- captureParam "source"
--     let e = parse beam
--     let tr = pt' e
--     let jsn = toJson tr
--     html $ pack (jsn)

main :: IO ()
main = scotty 3000 $ do
  middleware simpleCors
  parseService


