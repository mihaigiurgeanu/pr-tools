import Control.Monad (when)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (head, intercalate, length, notElem, null, (!!), sortBy, nub)
import Options.Applicative
import System.Exit (exitFailure, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess, readProcessWithExitCode)
import PRTools.Config (trimTrailing, getBaseBranch)
import PRTools.PRState
import PRTools.ContentHash (generatePatchHash, debugPatchDifference)
import Data.Time (getCurrentTime, formatTime)
import Data.Time.Format (defaultTimeLocale)

data Global = Global
  { gBaseBranch :: Maybe String
  }

data App = App Global Command

data Command =
    Approve { aBranch :: Maybe String, aBy :: String, aCommit :: Maybe String }
  | Status { sBranch :: Maybe String }
  | Record { rBranch :: Maybe String, rTip :: Maybe String }
  | List
  | Rebase { reBranch :: Maybe String, reOldCommit :: Maybe String, reNewCommit :: Maybe String }
  | Debug { dBranch :: Maybe String, dOldCommit :: String, dNewCommit :: Maybe String }
  | Migrate

-- Build ordered commit list: latest snapshot first, then earlier snapshots (filtering duplicates)
buildOrderedCommitList :: [PRSnapshot] -> [CommitInfo]
buildOrderedCommitList [] = []
buildOrderedCommitList snapshots = go snapshots Set.empty []
  where
    go [] _ acc = reverse acc
    go (snapshot:rest) seen acc = 
      let newCommits = filter (\ci -> not (Set.member (ciHash ci) seen)) (psCommits snapshot)
          newSeen = foldl (\s ci -> Set.insert (ciHash ci) s) seen newCommits
      in go rest newSeen (reverse newCommits ++ acc)

globalParser :: Parser Global
globalParser = Global
  <$> optional (strOption (long "base" <> metavar "COMMIT" <> help "Override base branch with specific commit"))

appParser :: Parser App
appParser = App <$> globalParser <*> commandParser

commandParser :: Parser Command
commandParser = subparser
  ( command "approve" (info (approveParser <**> helper) (progDesc "Approve a PR by name"))
 <> command "status" (info (statusParser <**> helper) (progDesc "Get PR status, approvals, and review history"))
 <> command "record" (info (recordParser <**> helper) (progDesc "Record/update PR commit snapshot"))
 <> command "update" (info (recordParser <**> helper) (progDesc "Synonym for record"))
 <> command "u" (info (recordParser <**> helper) (progDesc "Shortcut for update"))
 <> command "r" (info (recordParser <**> helper) (progDesc "Shortcut for record"))
 <> command "rec" (info (recordParser <**> helper) (progDesc "Shortcut for record"))
 <> command "list" (info (pure List <**> helper) (progDesc "List all tracked PRs with status"))
 <> command "rebase" (info (rebaseParser <**> helper) (progDesc "Transfer approvals after rebase if content unchanged"))
 <> command "debug" (info (debugParser <**> helper) (progDesc "Debug content hash differences between commits"))
 <> command "migrate" (info (pure Migrate <**> helper) (progDesc "Migrate legacy commit info to include timestamps"))
  )
  where
    approveParser = Approve
      <$> optional (strArgument (metavar "BRANCH" <> help "Branch to approve (default: current)"))
      <*> strOption (long "by" <> metavar "NAME" <> help "Approver name")
      <*> optional (strOption (long "commit" <> metavar "HASH" <> help "Commit hash being approved"))
    statusParser = Status
      <$> optional (strArgument (metavar "BRANCH" <> help "Branch to check (default: current)"))
    recordParser = Record
      <$> optional (strArgument (metavar "BRANCH" <> help "Branch to record (default: current)"))
      <*> optional (strOption (long "tip" <> metavar "TIP" <> help "The tip commit of the PR (default BRANCH)"))
    rebaseParser = Rebase
      <$> optional (strArgument (metavar "BRANCH" <> help "Branch that was rebased (default: current)"))
      <*> optional (strOption (long "old-commit" <> metavar "HASH" <> help "Commit hash before rebase (default: find from approval history)"))
      <*> optional (strOption (long "new-commit" <> metavar "HASH" <> help "Commit hash after rebase (default: current HEAD)"))
    debugParser = Debug
      <$> optional (strArgument (metavar "BRANCH" <> help "Branch to debug (default: current)"))
      <*> strOption (long "old-commit" <> metavar "HASH" <> help "Old commit hash")
      <*> optional (strOption (long "new-commit" <> metavar "HASH" <> help "New commit hash (default: current HEAD)"))

main :: IO ()
main = do
  App global cmd <- execParser $ info (appParser <**> helper) 
    ( fullDesc
   <> progDesc "Track PR approvals, status, and commit history"
   <> header "pr-track - PR tracking and approval management tool" )
  
  baseB <- case gBaseBranch global of
    Just b -> return b
    Nothing -> getBaseBranch
  
  state <- loadState
  case cmd of
    Approve mbBranch by mbCommit -> do
      branch <- case mbBranch of
        Just b -> return b
        Nothing -> fmap trimTrailing (readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] "")
      
      tip <- case mbCommit of
        Just c -> return c
        Nothing -> fmap trimTrailing (readProcess "git" ["rev-parse", branch] "")

      recordApprovalWithBase branch by tip baseB
      putStrLn $ "Approved " ++ branch ++ " by " ++ by
    Status mbBranch -> do
      branch <- case mbBranch of
        Just b -> return b
        Nothing -> fmap trimTrailing (readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] "")
      case Map.lookup branch state of
        Nothing -> putStrLn $ "No status for " ++ branch
        Just pr -> do
          putStrLn $ "Status: " ++ prStatus pr
          
          -- Check current valid approvals
          validApprovals <- checkValidApprovalsWithBase branch baseB
          let allApprovers = nub (map apApprover (approvalHistory pr))
          let validApprovers = nub (map apApprover validApprovals)
          
          putStr "Approvals: "
          if null allApprovers 
            then putStrLn "none"
            else do
              putStrLn $ intercalate ", " validApprovers
              when (length validApprovers < length allApprovers) $
                putStrLn $ "  (Note: " ++ show (length allApprovers - length validApprovers) ++ " approvals are no longer valid due to changes)"

          when (not (null (approvalHistory pr))) $ do
            putStrLn "Approval History:"
            mapM_ (\ap -> do
              let isValid = ap `elem` validApprovals
              let validityNote = if isValid then "" else " (INVALID - content changed)"
              putStrLn $ "- Approved by " ++ apApprover ap ++ " at " ++ apTimestamp ap ++ validityNote
              putStrLn "  Approved commits:"
              mapM_ (\c -> putStrLn $ "  - " ++ take 7 (ciHash c) ++ " " ++ ciMessage c) (apCommits ap)
              case apContentHash ap of
                Just hash -> putStrLn $ "  Content hash: " ++ hash
                Nothing -> putStrLn "  Content hash: (legacy approval)"
              ) (approvalHistory pr)

          -- Check review status
          (isReviewed, reviewers) <- checkReviewStatusWithBase branch baseB
          putStr "Review Status: "
          if isReviewed
            then putStrLn $ "Reviewed by " ++ intercalate ", " reviewers
            else putStrLn "Not reviewed"

          when (not (null (prReviews pr))) $ do
            putStrLn "Review History:"
            mapM_ (\re -> do
              let hashInfo = case reContentHash re of
                    Just hash -> " (content hash: " ++ hash ++ ")"
                    Nothing -> ""
              putStrLn $ "- " ++ reAction re ++ " by " ++ reReviewer re ++ " at " ++ reTimestamp re ++ hashInfo
              ) (prReviews pr)

          when (not (null (prFixes pr))) $ do
            putStrLn "Fix History:"
            mapM_ (\fe -> putStrLn $ "- " ++ feAction fe ++ " by " ++ feFixer fe ++ " at " ++ feTimestamp fe) (prFixes pr)

          case prMergeInfo pr of
            Just mi -> do
              putStrLn "Merge Info:"
              putStrLn $ "- Merged by " ++ miMerger mi ++ " at " ++ miTimestamp mi
              putStrLn "  Merged commits:"
              mapM_ (\c -> putStrLn $ "  - " ++ take 7 (ciHash c) ++ " " ++ ciMessage c) (miCommits mi)
            Nothing -> return ()

          if null (prSnapshots pr)
            then putStrLn "No snapshots recorded"
            else do
              (code, out, _) <- readProcessWithExitCode "git" ["rev-parse", "--verify", branch] ""
              let branch_exists = code == ExitSuccess
              let branch_tip = if branch_exists then trimTrailing out else ""
              
              -- Build ordered list: latest snapshot first, then earlier snapshots (no duplicates)
              let orderedCommits = buildOrderedCommitList (reverse (prSnapshots pr)) -- reverse to get latest first
              
              temp_list <- mapM (\ci -> do
                is_m <- isAncestor (ciHash ci) baseB
                is_p <- if not branch_exists then return False else isAncestor (ciHash ci) branch_tip
                let s = if is_m then "merged" else if is_p then "pending" else "removed"
                return (ci, s)
                ) orderedCommits
              let mergedCount = length [() | (_,s) <- temp_list, s == "merged"]
              let pendingCount = length [() | (_,s) <- temp_list, s == "pending"]
              let removedCount = length [() | (_,s) <- temp_list, s == "removed"]
              let total = length temp_list
              putStrLn $ "Latest snapshot at " ++ psTimestamp (last (prSnapshots pr))
              putStrLn $ "Commit status: " ++ show mergedCount ++ " merged, " ++ show pendingCount ++ " pending, " ++ show removedCount ++ " removed out of " ++ show total
              let latest = last (prSnapshots pr)
              let latest_hashes = map ciHash (psCommits latest)
              let hash_to_s = Map.fromList [ (ciHash ci, s) | (ci, s) <- temp_list ]
              let all_latest_merged = not (null latest_hashes) && all (\h -> Map.lookup h hash_to_s == Just "merged") latest_hashes
              if all_latest_merged
                then putStrLn $ "PR is fully merged into " ++ baseB
                else return ()
              putStrLn "All historical commits:"
              mapM_ (\(ci, s) -> do
                isReviewed <- checkCommitReviewStatusWithBase branch (ciHash ci) baseB
                let reviewStatus = if isReviewed then ", reviewed" else ", not reviewed"
                let boldStart = if s == "merged" || s == "pending" then "\ESC[1m" else ""
                let boldEnd = if s == "merged" || s == "pending" then "\ESC[0m" else ""
                putStrLn $ "- " ++ boldStart ++ take 7 (ciHash ci) ++ " " ++ ciMessage ci ++ " (" ++ s ++ reviewStatus ++ ")" ++ boldEnd
                ) temp_list
    Record mbBranch mbTip -> do
      branch <- case mbBranch of
        Just b -> return b
        Nothing -> fmap trimTrailing (readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] "")
      msg <- case mbTip of
        Just t -> recordPRWithBaseAndTip branch baseB t
        Nothing -> recordPRWithBase branch baseB
      putStrLn $ "Recorded snapshot for " ++ branch ++ (if null msg then "" else ". " ++ msg)
    List -> do
      if Map.null state
        then putStrLn "No PRs tracked"
        else do
          -- Convert to list and sort by most recent commit in latest snapshot
          let prList = Map.toList state
          sortedPRs <- mapM (\(branch, pr) -> do
            latestCommitTime <- if null (prSnapshots pr) 
              then return 0  -- Very old timestamp for PRs with no snapshots
              else do
                let latestSnapshot = last (prSnapshots pr)
                let commits = psCommits latestSnapshot
                if null commits
                  then return 0
                  else do
                    -- Get the latest commit timestamp from the latest snapshot
                    let commitTimestamps = map (\ci -> case ciTimestamp ci of
                          Just ts -> ts
                          Nothing -> 0  -- Fallback for legacy commits
                          ) commits
                    return (maximum commitTimestamps)
            return (branch, pr, latestCommitTime)
            ) prList
          
          let sortedByActivity = sortBy (\(_, _, time1) (_, _, time2) -> 
                compare time2 time1  -- Most recent first
                ) sortedPRs
          
          mapM_ (\(b, pr, _) -> do
            let status = prStatus pr
            let approvalCount = length $ approvalHistory pr
            let boldStart = if status == "open" || status == "stale" then "\ESC[1m" else ""
            let boldEnd = if status == "open" || status == "stale" then "\ESC[0m" else ""
            putStrLn $ boldStart ++ b ++ ": " ++ status ++ " (approvals: " ++ show approvalCount ++ ")" ++ boldEnd
            ) sortedByActivity
    Rebase mbBranch mbOldCommit mbNewCommit -> do
      branch <- case mbBranch of
        Just b -> return b
        Nothing -> fmap trimTrailing (readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] "")
      
      newCommit <- case mbNewCommit of
        Just c -> return c
        Nothing -> fmap trimTrailing (readProcess "git" ["rev-parse", branch] "")
      
      transferApprovalsAfterRebaseWithBase branch mbOldCommit newCommit baseB
    Debug mbBranch oldCommit mbNewCommit -> do
      branch <- case mbBranch of
        Just b -> return b
        Nothing -> fmap trimTrailing (readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] "")
      
      newCommit <- case mbNewCommit of
        Just c -> return c
        Nothing -> fmap trimTrailing (readProcess "git" ["rev-parse", branch] "")
      
      putStrLn $ "Debugging content hash difference between " ++ oldCommit ++ " and " ++ newCommit
      putStrLn $ "Base branch: " ++ baseB
      
      oldHash <- generatePatchHash baseB oldCommit
      newHash <- generatePatchHash baseB newCommit
      
      putStrLn $ "Old content hash: " ++ oldHash
      putStrLn $ "New content hash: " ++ newHash
      putStrLn $ "Hashes match: " ++ show (oldHash == newHash)
      
      debugPatchDifference baseB oldCommit newCommit
    Migrate -> do
      putStrLn "Migrating legacy commit data to include timestamps..."
      migratedState <- migrateCommitTimestamps state
      saveState migratedState
      putStrLn "Migration complete. All legacy commits now have timestamps."
