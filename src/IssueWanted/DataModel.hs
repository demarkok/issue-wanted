module IssueWanted.DataModel where

import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple.FromRow

data Repo = Repo {repoName :: String, repoUrl :: String} deriving (Show, Eq, Generic)
data Issue = Issue {issueName :: String, issueUrl :: String, description :: String} deriving (Show, Eq, Generic)

instance FromRow Repo
instance ToJSON Repo

instance FromRow Issue
instance ToJSON Issue