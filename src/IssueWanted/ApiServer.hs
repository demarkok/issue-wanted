module IssueWanted.ApiServer where

import IssueWanted.DataModel
import IssueWanted.QueryProcessor

import qualified Web.Scotty as S
import Database.PostgreSQL.Simple

apiServer :: Connection -> S.ScottyM ()
apiServer connection = do
    S.get "/api/getIssues" $ do
        repo <- S.param "repo"
        issues <- liftIO $ getIssues connection repo
        S.json issues 
    S.get "/api/repos" $ do
        repos <- liftIO $ getRepos connection
        S.json repos