module Main where

import Database.PostgreSQL.Simple

import Web.Scotty 
import Network.Wai.Middleware.Cors
import IssueWanted.Search
import IssueWanted.ApiServer


main :: IO ()
main = do
    conn <- connectPostgreSQL $ fromString "host='0.0.0.0' user='postgres' dbname='postgres'"
    scotty 1234 $ do
        middleware simpleCors
        apiServer conn
    return ()

