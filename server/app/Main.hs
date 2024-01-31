
module Main where
import Data.Text.Lazy
import Service

import qualified Alan.Lang

import Web.Scotty
import Network.Wai.Middleware.Cors
import qualified Network.Wai as WAI



corsPolicy :: CorsResourcePolicy
corsPolicy = CorsResourcePolicy
    { corsOrigins = Nothing
    , corsMethods = []
    , corsRequestHeaders = simpleHeaders
    , corsExposedHeaders = Just simpleResponseHeaders
    , corsMaxAge = Nothing
    , corsVaryOrigin = False
    , corsRequireOrigin = False
    , corsIgnoreFailures = False
    }

corsMW :: WAI.Middleware
corsMW = cors (const $ Just corsPolicy)


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
  middleware corsMW
  parseService
  get "/" $ html $ pack "Api working!"


