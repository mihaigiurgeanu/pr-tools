import Control.Monad (when)
import qualified Data.Map.Strict as Map
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
  | Record { rBranch :: Maybe String }
  | List
  | Rebase { reBranch :: Maybe String, reOldCommit :: String, reNewCommit :: Maybe String }
  | Debug { dBranch :: Maybe String, dOldCommit :: String, dNewCommit :: Maybe String }

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
    rebaseParser = Rebase
      <$> optional (strOption (long "branch" <> metavar "BRANCH" <> help "Branch that was rebased (default: current)"))
      <*> strOption (long "old-commit" <> metavar "HASH" <> help "Commit hash before rebase")
      <*> optional (strOption (long "new-commit" <> metavar "HASH" <> help "Commit hash after rebase (default: current HEAD)"))
    debugParser = Debug
      <$> optional (strOption (long "branch" <> metavar "BRANCH" <> help "Branch to debug (default: current)"))
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
              let all_commits = foldl (\m ps -> foldl (\m' ci -> if Map.member (ciHash ci) m' then m' else Map.insert (ciHash ci) (ci, psTimestamp ps) m') m (psCommits ps)) Map.empty (prSnapshots pr)
              temp_list <- mapM (\(ci, ts) -> do
                is_m <- isAncestor (ciHash ci) baseB
                is_p <- if not branch_exists then return False else isAncestor (ciHash ci) branch_tip
                let s = if is_m then "merged" else if is_p then "pending" else "removed"
                -- Get the actual commit timestamp from git
                commitTime <- trimTrailing <$> readProcess "git" ["log", "-1", "--format=%ct", ciHash ci] ""
                let commitTimestamp = read commitTime :: Int
                return (ci, s, ts, commitTimestamp)
                ) (Map.elems all_commits)
              let sorted_statuses = sortBy (\(_,_,_,t1) (_,_,_,t2) -> compare t2 t1) temp_list -- newest first
              let mergedCount = length [() | (_,s,_,_) <- sorted_statuses, s == "merged"]
              let pendingCount = length [() | (_,s,_,_) <- sorted_statuses, s == "pending"]
              let removedCount = length [() | (_,s,_,_) <- sorted_statuses, s == "removed"]
              let total = length sorted_statuses
              putStrLn $ "Latest snapshot at " ++ psTimestamp (last (prSnapshots pr))
              putStrLn $ "Commit status: " ++ show mergedCount ++ " merged, " ++ show pendingCount ++ " pending, " ++ show removedCount ++ " removed out of " ++ show total
              let latest = last (prSnapshots pr)
              let latest_hashes = map ciHash (psCommits latest)
              let hash_to_s = Map.fromList [ (ciHash ci, s) | (ci, s, _, _) <- temp_list ]
              let all_latest_merged = not (null latest_hashes) && all (\h -> Map.lookup h hash_to_s == Just "merged") latest_hashes
              if all_latest_merged
                then putStrLn $ "PR is fully merged into " ++ baseB
                else return ()
              putStrLn "All historical commits:"
              mapM_ (\(ci, s, _, _) -> do
                isReviewed <- checkCommitReviewStatusWithBase branch (ciHash ci) baseB
                let reviewStatus = if isReviewed then ", reviewed" else ", not reviewed"
                let boldStart = if s == "merged" || s == "pending" then "\ESC[1m" else ""
                let boldEnd = if s == "merged" || s == "pending" then "\ESC[0m" else ""
                putStrLn $ "- " ++ boldStart ++ take 7 (ciHash ci) ++ " " ++ ciMessage ci ++ " (" ++ s ++ reviewStatus ++ ")" ++ boldEnd
                ) sorted_statuses
    Record mbBranch -> do
      branch <- case mbBranch of
        Just b -> return b
        Nothing -> fmap trimTrailing (readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] "")
      msg <- recordPRWithBase branch baseB
      putStrLn $ "Recorded snapshot for " ++ branch ++ (if null msg then "" else ". " ++ msg)
    List -> do
      if Map.null state
        then putStrLn "No PRs tracked"
        else do
          -- Convert to list and sort by most recent activity
          let prList = Map.toList state
          sortedPRs <- mapM (\(branch, pr) -> do
            let lastActivity = if null (prSnapshots pr) 
                              then "1970-01-01 00:00:00"  -- Very old date for PRs with no snapshots
                              else psTimestamp (last (prSnapshots pr))
            return (branch, pr, lastActivity)
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
    Rebase mbBranch oldCommit mbNewCommit -> do
      branch <- case mbBranch of
        Just b -> return b
        Nothing -> fmap trimTrailing (readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] "")
      
      newCommit <- case mbNewCommit of
        Just c -> return c
        Nothing -> fmap trimTrailing (readProcess "git" ["rev-parse", branch] "")
      
      transferApprovalsAfterRebaseWithBase branch oldCommit newCommit baseB
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
