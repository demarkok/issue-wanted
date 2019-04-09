{-# LANGUAGE QuasiQuotes #-}

module IssueWanted.QueryProcessor  where

import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple
import IssueWanted.DataModel
import Database.PostgreSQL.Simple.SqlQQ


-- | Get the list of all relevant repositories
getRepos :: Connection -> IO [Repo]
getRepos connection = (query_ connection queryText :: IO [Repo]) where
  queryText = "SELECT name, url FROM repos"

-- | Get the list of all relevant issues in the given repository
getIssues :: Connection -> String -> IO [Issue]
getIssues connection repo = (query connection queryText [repo] :: IO [Issue]) where
  queryText = [sql| SELECT issues.name, issues.url, description
                      FROM issues
                      JOIN repos ON repos.id = issues.repo_id
                    WHERE repos.name = ? |]
