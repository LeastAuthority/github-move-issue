module Lib
    ( someFunc
    ) where

import qualified Data.Vector as Vector

import Control.Monad.IO.Class
  ( liftIO
  )

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
  ( Milestone(milestoneNumber, milestoneTitle)
  )

import GitHub.Auth
  ( Auth(OAuth)
  )

import GitHub.Data.Id
  ( Id
  , mkId
  )

import GitHub.Endpoints.Issues
  ( Issue(..)
  , NewIssue(..)
  , issue
  , createIssue
  )

import GitHub.Endpoints.Issues.Milestones
  ( milestones'
  )

someFunc
  :: Text    -- Personal authentication token.  This definitely must have at
             -- least the "repo" privilege!
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
move auth owner repo issue = do
  maybeMilestoneId <- maybeTranslatedMilestone auth owner repo issue
  let newIssue = (issueToNewIssue issue)
                 { newIssueMilestone = maybeMilestoneId
                 }
  putStrLn $ show newIssue
  createIssue auth owner repo newIssue


maybeTranslatedMilestone
  :: Auth                    -- Credentials for authentication
  -> Name Owner              -- The destination owner
  -> Name Repo               -- The destination repository
  -> Issue
  -> IO (Maybe (Id Milestone))
maybeTranslatedMilestone auth owner repo issue =
  case issueMilestone issue of
    Nothing              -> return Nothing
    Just sourceMilestone -> translateMilestone auth owner repo sourceMilestone

translateMilestone
  :: Auth                      -- Credentials for authentication
  -> Name Owner                -- The owner of the repository in which to seek a milestone
  -> Name Repo                 -- The repository in which to seek a milestone
  -> Milestone                 -- The milestone to seek a translation of
  -> IO (Maybe (Id Milestone)) -- The id of the a milestone matching the above.
translateMilestone auth owner repo sourceMilestone = do
  -- Look up the milestones and find one with a matching title
  result <- milestones' (Just auth) owner repo
  return $ either (const Nothing) ((fmap milestoneNumber) . findMilestoneWithTitle (milestoneTitle sourceMilestone)) result
  where
    findMilestoneWithTitle :: Text -> Vector.Vector Milestone -> Maybe Milestone
    findMilestoneWithTitle title = Vector.find ((== title) . milestoneTitle)

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
