module Lib
    ( someFunc
    ) where

import qualified Data.Vector as Vector

import Text.Printf
  ( printf
  )

import Data.Proxy
  ( Proxy(Proxy)
  )

import Data.Text (
  Text
  )
import Data.Text.Encoding
  ( encodeUtf8
  )


import GitHub.Data.Definitions
  ( Error
  , Organization(Organization)
  , Owner(Owner)
  , IssueLabel(labelName)
  , SimpleUser(simpleUserLogin)
  )

import GitHub.Data.Repos
  ( Repo(Repo)
  )

import GitHub.Data.Name
  ( Name
  , mkName
  , untagName
  )

import GitHub.Data.Milestone
  ( Milestone(milestoneNumber)
  )

import GitHub.Auth
  ( Auth(OAuth)
  )

import GitHub.Endpoints.Issues
  ( Issue(..)
  , NewIssue(..)
  , issue
  , createIssue
  , mkId
  )

someFunc
  :: Text    -- Personal authentication token
  -> Text    -- Source owner name
  -> Text    -- Source repository name
  -> Integer -- Source issue number
  -> Text    -- Destination owner name
  -> Text    -- Destination repository name
  -> IO ()   -- The issue might have been moved
someFunc token sourceOwner sourceRepo sourceIssue destOwner destRepo = do
  let auth = OAuth $ encodeUtf8 token
  moving <- issue
    (mkName (Proxy :: Proxy Owner) sourceOwner)
    (mkName (Proxy :: Proxy Repo) sourceRepo)
    (mkId (Proxy :: Proxy Issue) (fromInteger sourceIssue))

  case moving of
    Left err -> putStrLn "oh no"
    Right issue -> do
      result <- move
        auth
        (mkName (Proxy :: Proxy Owner) destOwner)
        (mkName (Proxy :: Proxy Repo) destRepo)
        issue

      case result of
        Left  err   -> putStrLn $ printf "Create failed: %s" $ show err
        Right moved -> putStrLn "ok"

move
  :: Auth                    -- Credentials for authentication
  -> Name Owner              -- The destination owner
  -> Name Repo               -- The destination repository
  -> Issue                   -- The issue to "move
  -> IO (Either Error Issue) -- The issue might have been moved
move auth owner repo issue =
  createIssue auth owner repo $ issueToNewIssue issue


issueToNewIssue
  :: Issue
  -> NewIssue
issueToNewIssue oldIssue =
  NewIssue
  { newIssueTitle = issueTitle oldIssue
  , newIssueBody = issueBody oldIssue
  , newIssueAssignee = case Vector.length $ issueAssignees oldIssue of
      0 -> Nothing
      x -> Just $ untagName $ simpleUserLogin $ Vector.head $ issueAssignees oldIssue
  , newIssueMilestone = milestoneNumber <$> issueMilestone oldIssue
  , newIssueLabels = Just $ Vector.map labelName $ issueLabels oldIssue
  }
