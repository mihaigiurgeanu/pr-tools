module PRTools.PRState where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Aeson.Key (fromString)
import Data.Aeson.Types ((.!=))
import qualified Data.Map.Strict as Map
import Data.Yaml (FromJSON(..), decodeFileEither, encodeFile, parseJSON, withObject, (.:), (.:?))
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Data.Time (UTCTime, NominalDiffTime, diffUTCTime, formatTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import System.Process (readProcess, readProcessWithExitCode)
import PRTools.Config (getBaseBranch, trimTrailing, getStaleDays)
import PRTools.ContentHash (generatePatchHash)
import System.Exit (ExitCode(..))
import Data.List (nub)

data CommitInfo = CommitInfo 
  { ciHash :: String
  , ciMessage :: String
  , ciTimestamp :: Maybe Int  -- Unix timestamp, Nothing for legacy commits
  } deriving (Eq, Show)

instance FromJSON CommitInfo where
  parseJSON = withObject "CommitInfo" $ \v -> CommitInfo
    <$> v .: fromString "hash"
    <*> v .: fromString "message"
    <*> v .:? fromString "timestamp"

instance ToJSON CommitInfo where
  toJSON ci = object
    [ fromString "hash" .= ciHash ci
    , fromString "message" .= ciMessage ci
    , fromString "timestamp" .= ciTimestamp ci
    ]

data PRSnapshot = PRSnapshot 
  { psTimestamp :: String
  , psCommits :: [CommitInfo]
  , psContentHash :: Maybe String  -- Content hash for the snapshot
  } deriving (Eq, Show)

instance FromJSON PRSnapshot where
  parseJSON = withObject "PRSnapshot" $ \v -> PRSnapshot
    <$> v .: fromString "timestamp"
    <*> v .: fromString "commits"
    <*> v .:? fromString "content_hash"

instance ToJSON PRSnapshot where
  toJSON ps = object
    [ fromString "timestamp" .= psTimestamp ps
    , fromString "commits" .= psCommits ps
    , fromString "content_hash" .= psContentHash ps
    ]

data ReviewEvent = ReviewEvent
  { reReviewer :: String
  , reAction :: String -- "start", "end", "import-answers"
  , reTimestamp :: String
  , reContentHash :: Maybe String -- Content hash for "end" actions (legacy)
  , reReviewedCommits :: Maybe (Map.Map String String) -- commit hash -> content hash for that commit
  } deriving (Eq, Show)

instance FromJSON ReviewEvent where
  parseJSON = withObject "ReviewEvent" $ \v -> ReviewEvent
    <$> v .: fromString "reviewer"
    <*> v .: fromString "action"
    <*> v .: fromString "timestamp"
    <*> v .:? fromString "content_hash"
    <*> v .:? fromString "reviewed_commits"

instance ToJSON ReviewEvent where
  toJSON re = object
    [ fromString "reviewer" .= reReviewer re
    , fromString "action" .= reAction re
    , fromString "timestamp" .= reTimestamp re
    , fromString "content_hash" .= reContentHash re
    , fromString "reviewed_commits" .= reReviewedCommits re
    ]

data FixEvent = FixEvent
  { feFixer :: String
  , feAction :: String -- "start", "end"
  , feTimestamp :: String
  } deriving (Eq, Show)

instance FromJSON FixEvent where
  parseJSON = withObject "FixEvent" $ \v -> FixEvent
    <$> v .: fromString "fixer"
    <*> v .: fromString "action"
    <*> v .: fromString "timestamp"

instance ToJSON FixEvent where
  toJSON fe = object
    [ fromString "fixer" .= feFixer fe
    , fromString "action" .= feAction fe
    , fromString "timestamp" .= feTimestamp fe
    ]

data Approval = Approval
  { apApprover :: String
  , apTimestamp :: String
  , apCommits :: [CommitInfo]
  , apContentHash :: Maybe String  -- New field for content-based approval
  } deriving (Eq, Show)

instance FromJSON Approval where
    parseJSON = withObject "Approval" $ \v -> Approval
        <$> v .: fromString "approver"
        <*> v .: fromString "timestamp"
        <*> v .:? fromString "commits" .!= []
        <*> v .:? fromString "content_hash"

instance ToJSON Approval where
    toJSON ap = object
        [ fromString "approver" .= apApprover ap
        , fromString "timestamp" .= apTimestamp ap
        , fromString "commits" .= apCommits ap
        , fromString "content_hash" .= apContentHash ap
        ]

data MergeInfo = MergeInfo
  { miMerger :: String
  , miTimestamp :: String
  , miCommits :: [CommitInfo]
  } deriving (Eq, Show)

instance FromJSON MergeInfo where
  parseJSON = withObject "MergeInfo" $ \v -> MergeInfo
    <$> v .: fromString "merger"
    <*> v .: fromString "timestamp"
    <*> v .: fromString "commits"

instance ToJSON MergeInfo where
  toJSON mi = object
    [ fromString "merger" .= miMerger mi
    , fromString "timestamp" .= miTimestamp mi
    , fromString "commits" .= miCommits mi
    ]

data PRState = PRState
  { prStatus :: String
  , prSnapshots :: [PRSnapshot]
  , approvalHistory :: [Approval]
  , prReviews :: [ReviewEvent]
  , prFixes :: [FixEvent]
  , prMergeInfo :: Maybe MergeInfo
  } deriving (Eq, Show)

instance FromJSON PRState where
  parseJSON = withObject "PRState" $ \v -> do
    maybeNewApprovals <- v .:? fromString "approval_history"
    maybeIntermediateApprovals <- v .:? fromString "approvals2"
    maybeOldApprovals <- v .:? fromString "approvals" .!= []

    let finalApprovals = case maybeNewApprovals of
                           Just new -> new
                           Nothing -> case maybeIntermediateApprovals of
                                        Just intermediate -> intermediate
                                        Nothing -> map (\name -> Approval name "migrated-approval" [] Nothing) maybeOldApprovals

    PRState
      <$> v .: fromString "status"
      <*> v .:? fromString "snapshots" .!= []
      <*> pure finalApprovals
      <*> v .:? fromString "reviews" .!= []
      <*> v .:? fromString "fixes" .!= []
      <*> v .:? fromString "merge_info"


instance ToJSON PRState where
  toJSON p = object
    [ fromString "status" .= prStatus p
    , fromString "snapshots" .= prSnapshots p
    , fromString "approval_history" .= approvalHistory p
    , fromString "reviews" .= prReviews p
    , fromString "fixes" .= prFixes p
    , fromString "merge_info" .= prMergeInfo p
    ]

statePath :: FilePath
statePath = ".pr-state.yaml"

loadState :: IO (Map.Map String PRState)
loadState = do
  exists <- doesFileExist statePath
  if exists
    then do
      res <- decodeFileEither statePath
      case res of
        Left err -> do
          hPutStrLn stderr (show err)
          exitFailure
        Right val -> return val
    else return Map.empty

saveState :: Map.Map String PRState -> IO ()
saveState = encodeFile statePath

isAncestor :: String -> String -> IO Bool
isAncestor commit branch = do
  (code, _, _) <- readProcessWithExitCode "git" ["merge-base", "--is-ancestor", commit, branch] ""
  return (code == ExitSuccess)

-- Helper function to extract CommitInfo from git log between base and tip
getCommitInfoBetween :: String -> String -> IO [CommitInfo]
getCommitInfoBetween base tip = do
  logOut <- readProcess "git" ["log", "--format=%H %s", base ++ ".." ++ tip, "--"] ""
  let commitLines = lines logOut
  mapM (\ln -> do
    let h = take 40 ln
    let m = drop 41 ln
    timestampStr <- trimTrailing <$> readProcess "git" ["log", "-1", "--format=%ct", h] ""
    let timestamp = read timestampStr :: Int
    return (CommitInfo h m (Just timestamp))
    ) (filter (not . null) commitLines)

recordPR :: String -> IO String
recordPR branch = do
  base <- getBaseBranch
  recordPRWithBase branch base

recordPRWithBase :: String -> String -> IO String
recordPRWithBase branch base = recordPRWithBaseAndTip branch base branch


-- Helper function to update PR state with new commits and determine status
updatePRStateWithCommits :: PRState -> String -> String -> String -> [CommitInfo] -> Bool -> IO (PRState, String)
updatePRStateWithCommits existingState branch base tip commits branchExists = do
  currentTime <- getCurrentTime
  let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
  
  -- Generate content hash for the snapshot if we have commits
  contentHash <- if null commits
    then return Nothing
    else do
      hash <- generatePatchHash base tip
      return (Just hash)
  
  let updatedSnapshots = if null commits
                         then prSnapshots existingState
                         else prSnapshots existingState ++ [PRSnapshot timeStr commits contentHash]

  staleDays <- getStaleDays
  let snapsForStale = if null updatedSnapshots then prSnapshots existingState else updatedSnapshots
  let isStale = case snapsForStale of
                  [] -> False
                  snaps -> let lastSnap = last snaps
                               lastTimeM = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (psTimestamp lastSnap)
                           in case lastTimeM of
                                Just lastTime -> diffUTCTime currentTime lastTime > fromIntegral (staleDays * 86400) && null commits
                                Nothing -> False
  let statusAfterStale = if isStale && prStatus existingState /= "stale" && prStatus existingState /= "merged" then "stale" else prStatus existingState

  let checkCommits = if not (null commits) then commits else if null (prSnapshots existingState) then [] else psCommits (last (prSnapshots existingState))
  let
    reachable :: String -> IO Bool
    reachable commit = and <$> mapM (\ci -> isAncestor (ciHash ci) commit) checkCommits
  allMerged <- if null checkCommits then return False else reachable base
  
   -- Check if all commits are removed (only for existing PRs)
  allRemoved <- if null checkCommits || prStatus existingState == "open" then return False else
    if branchExists then do
      -- Branch exists, check if commits are reachable from it
      branchReachable <- reachable branch
      return (not branchReachable)
    else if tip /= branch then do
           -- Branch does not exist, but we are using a custom tip
           tipReachable <- reachable tip
           return (not tipReachable)
         else do
           -- Branch doesn't exist, commits are removed if not in base
           baseReachable <- reachable base
           return (not baseReachable)
  
  let finalStatus = if allMerged then "merged" 
                   else if allRemoved then "removed"
                   else statusAfterStale

  let updated = existingState { prSnapshots = updatedSnapshots, prStatus = finalStatus }
  let msg = if finalStatus /= prStatus existingState
            then "Status updated to " ++ finalStatus
            else if null commits
                 then "No update performed"
                 else ""
  let extraMsg = if not branchExists then " (branch does not exist; using latest snapshot for checks)" else ""
  return (updated, msg ++ extraMsg)

recordPRWithBaseAndTip :: String -> String -> String -> IO String
recordPRWithBaseAndTip branch base tip = do
  state <- loadState
  let existing = Map.lookup branch state
  (branchExists, commits) <- do
    (code, out, _) <- readProcessWithExitCode "git" ["rev-parse", "--verify", tip] ""
    cs <- if code == ExitSuccess
      then getCommitInfoBetween base tip
      else return []
      
    (code, out, _) <- readProcessWithExitCode "git" ["rev-parse", "--verify", branch] ""
    if code == ExitSuccess
      then return (True, cs)
      else return (False, cs)

  case existing of
    Nothing -> do
      if not branchExists
        then do
          hPutStrLn stderr $ "Branch " ++ branch ++ " does not exist and is not tracked."
          exitFailure
        else do
          let initialState = PRState "open" [] [] [] [] Nothing
          (updated, msg) <- updatePRStateWithCommits initialState branch base tip commits branchExists
          let newState = Map.insert branch updated state
          saveState newState
          return $ msg ++ " (new tracking started)"
    Just ex -> do
      (updated, msg) <- updatePRStateWithCommits ex branch base tip commits branchExists
      let newState = Map.insert branch updated state
      saveState newState
      return msg

recordReviewEvent :: String -> String -> String -> IO ()
recordReviewEvent branch reviewer action = do
    state <- loadState
    currentTime <- getCurrentTime
    let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
    let newEvent = ReviewEvent reviewer action timeStr Nothing Nothing
    let pr = Map.findWithDefault (PRState "open" [] [] [] [] Nothing) branch state
    let updatedPr = pr { prReviews = prReviews pr ++ [newEvent] }
    let newState = Map.insert branch updatedPr state
    saveState newState

-- Record review event with content hash (for "end" actions)
recordReviewEventWithHash :: String -> String -> String -> Maybe String -> IO ()
recordReviewEventWithHash branch reviewer action mbContentHash = do
    state <- loadState
    currentTime <- getCurrentTime
    let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
    let newEvent = ReviewEvent reviewer action timeStr mbContentHash Nothing
    let pr = Map.findWithDefault (PRState "open" [] [] [] [] Nothing) branch state
    let updatedPr = pr { prReviews = prReviews pr ++ [newEvent] }
    let newState = Map.insert branch updatedPr state
    saveState newState

-- Record review event with individual commit hashes (for "end" actions)
recordReviewEventWithCommitHashes :: String -> String -> String -> Map.Map String String -> IO ()
recordReviewEventWithCommitHashes branch reviewer action commitHashes = do
    state <- loadState
    currentTime <- getCurrentTime
    let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
    let newEvent = ReviewEvent reviewer action timeStr Nothing (Just commitHashes)
    let pr = Map.findWithDefault (PRState "open" [] [] [] [] Nothing) branch state
    let updatedPr = pr { prReviews = prReviews pr ++ [newEvent] }
    let newState = Map.insert branch updatedPr state
    saveState newState

recordFixEvent :: String -> String -> String -> IO ()
recordFixEvent branch fixer action = do
    state <- loadState
    currentTime <- getCurrentTime
    let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
    let newEvent = FixEvent fixer action timeStr
    let pr = Map.findWithDefault (PRState "open" [] [] [] [] Nothing) branch state
    let updatedPr = pr { prFixes = prFixes pr ++ [newEvent] }
    let newState = Map.insert branch updatedPr state
    saveState newState

-- Check if approvals are still valid for the current branch state
checkValidApprovals :: String -> IO [Approval]
checkValidApprovals branch = do
  base <- getBaseBranch
  checkValidApprovalsWithBase branch base

checkValidApprovalsWithBase :: String -> String -> IO [Approval]
checkValidApprovalsWithBase branch base = do
  state <- loadState
  case Map.lookup branch state of
    Nothing -> return []
    Just pr -> do
      (code, out, _) <- readProcessWithExitCode "git" ["rev-parse", branch] ""
      if code /= ExitSuccess
        then return [] -- Branch doesn't exist, no valid approvals
        else do
          let currentCommit = trimTrailing out
          currentContentHash <- generatePatchHash base currentCommit
          
          let approvals = approvalHistory pr
          validApprovals <- filterM (\approval -> do
            -- Check if approval is valid by commit ID or content hash
            let commitValid = any (\ci -> ciHash ci == currentCommit) (apCommits approval)
            let contentValid = case apContentHash approval of
                  Just hash -> hash == currentContentHash
                  Nothing -> False
            -- For debugging, let's see what's happening
            -- putStrLn $ "Checking approval by " ++ apApprover approval ++ ": commitValid=" ++ show commitValid ++ ", contentValid=" ++ show contentValid
            return (commitValid || contentValid)
            ) approvals
          
          return validApprovals
  where
    filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
    filterM _ [] = return []
    filterM p (x:xs) = do
      flg <- p x
      ys <- filterM p xs
      return (if flg then x:ys else ys)

-- Transfer approvals after a rebase by updating commit hashes while preserving content hashes
transferApprovalsAfterRebase :: String -> Maybe String -> String -> IO ()
transferApprovalsAfterRebase branch mbOldCommit newCommit = do
  base <- getBaseBranch
  transferApprovalsAfterRebaseWithBase branch mbOldCommit newCommit base

transferApprovalsAfterRebaseWithBase :: String -> Maybe String -> String -> String -> IO ()
transferApprovalsAfterRebaseWithBase branch mbOldCommit newCommit base = do
  state <- loadState
  case Map.lookup branch state of
    Nothing -> putStrLn "No PR state found for branch"
    Just pr -> do
      -- Find old commit from approval history if not provided
      oldCommit <- case mbOldCommit of
        Just commit -> return commit
        Nothing -> do
          let approvals = approvalHistory pr
          if null approvals
            then do
              putStrLn "No approvals found to transfer from"
              exitFailure
            else do
              -- Use the most recent approval's first commit
              let latestApproval = last approvals
              let commits = apCommits latestApproval
              if null commits
                then do
                  putStrLn "No commits found in latest approval"
                  exitFailure
                else do
                  let oldCommit = ciHash (head commits)
                  putStrLn $ "Using commit from latest approval: " ++ take 7 oldCommit
                  return oldCommit
      
      oldContentHash <- generatePatchHash base oldCommit
      newContentHash <- generatePatchHash base newCommit
      
      putStrLn $ "Old content hash: " ++ oldContentHash
      putStrLn $ "New content hash: " ++ newContentHash
      
      -- Only transfer if content is the same
      if oldContentHash == newContentHash then do
        -- Get new commit list
        newCommits <- getCommitInfoBetween base newCommit
        
        putStrLn $ "Found " ++ show (length (approvalHistory pr)) ++ " approvals to check"
        
        -- Update approvals with new commit hashes but keep content hash
        let updatedApprovals = map (\approval -> 
              case apContentHash approval of
                Just hash | hash == oldContentHash -> do
                  let updated = approval { apCommits = newCommits }
                  updated
                _ -> approval
              ) (approvalHistory pr)
        
        -- Create a new approval entry recording the rebase transfer
        currentTime <- getCurrentTime
        let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
        let rebaseTransferApproval = Approval 
              { apApprover = "system:rebase-transfer"
              , apTimestamp = timeStr
              , apCommits = [CommitInfo oldCommit ("rebase transfer" ++ newCommit) Nothing]
              , apContentHash = Just oldContentHash
              }
        
        let finalApprovals = updatedApprovals ++ [rebaseTransferApproval]
        let updatedPr = pr { approvalHistory = finalApprovals }
        let newState = Map.insert branch updatedPr state
        saveState newState
        putStrLn $ "Approvals transferred after rebase. Updated " ++ show (length updatedApprovals) ++ " approvals."
        putStrLn $ "Added rebase transfer record: " ++ take 7 oldCommit ++ " -> " ++ take 7 newCommit
      else
        putStrLn "Content has changed - approvals cannot be transferred automatically"

recordApproval :: String -> String -> String -> IO ()
recordApproval branch approver commitHash = do
  base <- getBaseBranch
  recordApprovalWithBase branch approver commitHash base

recordApprovalWithBase :: String -> String -> String -> String -> IO ()
recordApprovalWithBase branch approver commitHash base = do
  state <- loadState
  let pr = Map.findWithDefault (PRState "open" [] [] [] [] Nothing) branch state
  
  commits <- getCommitInfoBetween base commitHash

  -- Generate content hash for the patch
  contentHash <- generatePatchHash base commitHash

  currentTime <- getCurrentTime
  let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
  let newApproval = Approval approver timeStr commits (Just contentHash)
  let newApprovalHistory = approvalHistory pr ++ [newApproval]
  
  let newPr = pr { approvalHistory = newApprovalHistory }
  let newState = Map.insert branch newPr state
  saveState newState

-- Check if current content has been reviewed
checkReviewStatus :: String -> IO (Bool, [String])
checkReviewStatus branch = do
  base <- getBaseBranch
  checkReviewStatusWithBase branch base

checkReviewStatusWithBase :: String -> String -> IO (Bool, [String])
checkReviewStatusWithBase branch base = do
  state <- loadState
  case Map.lookup branch state of
    Nothing -> return (False, [])
    Just pr -> do
      (code, out, _) <- readProcessWithExitCode "git" ["rev-parse", branch] ""
      if code /= ExitSuccess
        then do
          -- Branch doesn't exist, check if all non-removed commits from latest snapshot are reviewed
          if null (prSnapshots pr)
            then return (False, [])
            else do
              let latestSnapshot = last (prSnapshots pr)
              let commits = psCommits latestSnapshot
              if null commits
                then return (False, [])
                else do
                  -- Filter out removed commits (commits that no longer exist)
                  existingCommits <- filterM (\ci -> do
                    (commitCode, _, _) <- readProcessWithExitCode "git" ["rev-parse", "--verify", ciHash ci] ""
                    return (commitCode == ExitSuccess)
                    ) commits
                  
                  if null existingCommits
                    then return (True, []) -- All commits are removed, consider reviewed
                    else do
                      -- Check if all existing commits are reviewed
                      reviewStatuses <- mapM (checkCommitReviewStatus branch . ciHash) existingCommits
                      let allReviewed = and reviewStatuses
                      if allReviewed
                        then do
                          -- Get reviewers from review events
                          let reviewEvents = prReviews pr
                          let endEvents = filter (\re -> reAction re == "end") reviewEvents
                          let reviewers = nub [reReviewer re | re <- endEvents]
                          return (True, reviewers)
                        else return (False, [])
        else do
          -- Branch exists, get current commits and check if all are reviewed
          let currentCommit = trimTrailing out
          logOut <- readProcess "git" ["log", "--format=%H", base ++ ".." ++ currentCommit] ""
          let commitHashes = lines logOut
          if null commitHashes
            then return (True, []) -- No commits to review
            else do
              -- Check if all current commits are reviewed
              reviewStatuses <- mapM (checkCommitReviewStatus branch) commitHashes
              let allReviewed = and reviewStatuses
              if allReviewed
                then do
                  -- Get reviewers from review events
                  let reviewEvents = prReviews pr
                  let endEvents = filter (\re -> reAction re == "end") reviewEvents
                  let reviewers = nub [reReviewer re | re <- endEvents]
                  return (True, reviewers)
                else return (False, [])
  where
    filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
    filterM _ [] = return []
    filterM p (x:xs) = do
      flg <- p x
      ys <- filterM p xs
      return (if flg then x:ys else ys)

-- Migration functions to add timestamps to legacy commits
migrateCommitTimestamps :: Map.Map String PRState -> IO (Map.Map String PRState)
migrateCommitTimestamps state = do
  putStrLn $ "Migrating " ++ show (Map.size state) ++ " PRs..."
  Map.traverseWithKey (\branch pr -> do
    putStrLn $ "Migrating PR: " ++ branch
    migratePRCommits pr
    ) state

migratePRCommits :: PRState -> IO PRState
migratePRCommits pr = do
  -- Migrate snapshots
  migratedSnapshots <- mapM migrateSnapshot (prSnapshots pr)
  -- Migrate approval history
  migratedApprovals <- mapM migrateApproval (approvalHistory pr)
  -- Migrate merge info
  migratedMergeInfo <- case prMergeInfo pr of
    Just mi -> Just <$> migrateMergeInfo mi
    Nothing -> return Nothing
  
  return pr { prSnapshots = migratedSnapshots
            , approvalHistory = migratedApprovals  
            , prMergeInfo = migratedMergeInfo }

migrateSnapshot :: PRSnapshot -> IO PRSnapshot
migrateSnapshot ps = do
  migratedCommits <- mapM migrateCommitInfo (psCommits ps)
  return ps { psCommits = migratedCommits, psContentHash = psContentHash ps }

migrateApproval :: Approval -> IO Approval
migrateApproval ap = do
  migratedCommits <- mapM migrateCommitInfo (apCommits ap)
  return ap { apCommits = migratedCommits }

migrateMergeInfo :: MergeInfo -> IO MergeInfo
migrateMergeInfo mi = do
  migratedCommits <- mapM migrateCommitInfo (miCommits mi)
  return mi { miCommits = migratedCommits }

migrateCommitInfo :: CommitInfo -> IO CommitInfo
migrateCommitInfo ci = case ciTimestamp ci of
  Just _ -> return ci  -- Already has timestamp
  Nothing -> do
    -- Try to get timestamp from git
    (code, out, _) <- readProcessWithExitCode "git" ["log", "-1", "--format=%ct", ciHash ci] ""
    if code == ExitSuccess
      then do
        let timestamp = read (trimTrailing out) :: Int
        putStrLn $ "  Migrated commit " ++ take 7 (ciHash ci) ++ " with timestamp " ++ show timestamp
        return ci { ciTimestamp = Just timestamp }
      else do
        -- Commit doesn't exist anymore, use epoch time as fallback
        putStrLn $ "  Commit " ++ take 7 (ciHash ci) ++ " not found, using epoch time"
        return ci { ciTimestamp = Just 0 }

-- Check if a specific commit has been reviewed
checkCommitReviewStatus :: String -> String -> IO Bool
checkCommitReviewStatus branch commitHash = do
  base <- getBaseBranch
  checkCommitReviewStatusWithBase branch commitHash base

checkCommitReviewStatusWithBase :: String -> String -> String -> IO Bool
checkCommitReviewStatusWithBase branch commitHash base = do
  state <- loadState
  case Map.lookup branch state of
    Nothing -> return False
    Just pr -> do
      let reviewEvents = prReviews pr
      let endEvents = filter (\re -> reAction re == "end") reviewEvents
      
      -- Check new format first (individual commit hashes)
      let reviewedCommitMaps = [commitMap | re <- endEvents, Just commitMap <- [reReviewedCommits re]]
      let directlyReviewed = any (Map.member commitHash) reviewedCommitMaps
      
      if directlyReviewed
        then return True
        else do
          -- Fall back to legacy format for backward compatibility
          (code, _, _) <- readProcessWithExitCode "git" ["rev-parse", "--verify", commitHash] ""
          if code /= ExitSuccess
            then return False -- Commit doesn't exist
            else do
              let legacyHashes = [hash | re <- endEvents, Just hash <- [reContentHash re], reReviewedCommits re == Nothing]
              if null legacyHashes
                then return False
                else do
                  -- For legacy events, check if this commit's content matches any reviewed content
                  commitContentHash <- generatePatchHash base commitHash
                  return (commitContentHash `elem` legacyHashes)
