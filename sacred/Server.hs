{-# LANGUAGE OverloadedStrings #-}
module Server where

import Logic
import Types
import Web.Scotty
import Data.Monoid (mconcat)

startServer :: IO ()
startServer = scotty 8000 server

server :: ScottyM ()
server = do
    post "/analyse" $ do
        setHeader "Access-Control-Allow-Origin" "*"
        j <- jsonData
        json (respondAnalysis (j :: AnalysisReq))
    post "/review" $ do
        setHeader "Access-Control-Allow-Origin" "*"
        j <- jsonData
        json (respondReview (j :: ReviewReq))


