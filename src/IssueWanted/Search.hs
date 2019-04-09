{-# LANGUAGE OverloadedStrings #-}

module IssueWanted.Search where

import GitHub (Error, Repo, SearchResult, Issue)
import GitHub.Endpoints.Search

import qualified Data.Text as T

-- | Fetch all repositories with Haskell language
fetchAllHaskellRepos :: IO (Either Error (SearchResult Repo))
fetchAllHaskellRepos = searchRepos "language:haskell"

-- | Fetch all repositories with Haskell language and label "help-wanted"
fetchHaskellReposHW :: IO (Either Error (SearchResult Repo))
fetchHaskellReposHW = searchRepos "language:haskell help-wanted-issues:>0"

-- | Fetch all repositories with Haskell language and label "good-first-issue"
fetchHaskellReposGFI :: IO (Either Error (SearchResult Repo))
fetchHaskellReposGFI = searchRepos "language:haskell good-first-issues:>0"

-- | Fetch all issues with Haskell language and label "help-wanted"
fetchHelpWanted :: IO (Either Error (SearchResult Issue))
fetchHelpWanted = searchIssues "language:haskell label:\"help wanted\""

-- | Fetch all issues with Haskell language and label "good-first-issue"
fetchGoodFirstIssue :: IO (Either Error (SearchResult Issue))
fetchGoodFirstIssue = searchIssues "language:haskell label:\"good first issue\""


fetchAll :: IO [(Repo, [Issue])]
fetchAll = do
  possibleRepos <- fetchHaskellReposGFI
  let repoList = either (const []) (toList . searchResultResults) possibleRepos
  sequence $ do
    repo <- repoList
    let owner = T.unpack $ untagName $ simpleOwnerLogin $ repoOwner repo
    let name = T.unpack $ untagName $ repoName repo
    traceM $ "Fetching repository " ++ name ++ "..."
    return $ do
        possibleIssues <- searchIssues $ toText $ "language:haskell label:\"good first issue\"" ++ " repo:" ++ owner ++ "/" ++ name
        let issueList = either (const []) (toList . searchResultResults) possibleIssues
        return (repo, issueList)
