{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import IssueWanted.Search
import Prelude
import Database.PostgreSQL.Simple
import GitHub


-- | Initialize database
main :: IO ()
main = do
  conn <- connectPostgreSQL $ fromString "host='0.0.0.0' user='postgres' dbname='postgres'"

  possibleRepos <- fetchHaskellReposHW
  let reposList = either (const []) (toList . searchResultResults) possibleRepos

  possibleIssues <- fetchHelpWanted
  let issuesList = either (const []) (toList . searchResultResults) possibleIssues

  repoList <- fetchAll
  sequence $ do
    (repo, issues) <- repoList
    let repo_id = untagId $ repoId repo
    let repo_url = getUrl $ repoHtmlUrl repo
    let repo_name = untagName $ repoName repo
    let issue_inserts = do
          issue <- issues
          let issue_id = untagId $ issueId issue
          let issue_url = fromMaybe "" $ getUrl <$> (issueHtmlUrl issue)
          let issue_name = issueTitle issue
          let issue_desc = fromMaybe "" $ issueBody issue
          return $ execute conn "INSERT INTO issues (id, url, name, repo_id, description) VALUES (?,?,?,?,?)" (issue_id, issue_url, issue_name, repo_id, issue_desc)
    guard $ not $ null issue_inserts
    return $ sequence $ execute conn "INSERT INTO repos (id, url, name) VALUES (?,?,?)" (repo_id, repo_url, repo_name) : issue_inserts

  return ()
