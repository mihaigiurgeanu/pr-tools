import Control.Monad (when)
import qualified Data.Map.Strict as Map
import Data.List (head, intercalate, length, notElem, null, (!!), sortBy)
import Options.Applicative
import System.Exit (exitFailure, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess, readProcessWithExitCode)
import PRTools.Config (trimTrailing, getBaseBranch)
import PRTools.PRState

data Command =
    Approve { aBranch :: Maybe String, aBy :: String }
  | Status { sBranch :: Maybe String }
  | Record { rBranch :: Maybe String }
  | List

commandParser :: Parser Command
commandParser = subparser
  ( command "approve" (info approveParser (progDesc "Approve a PR"))
 <> command "status" (info statusParser (progDesc "Get PR status"))
 <> command "record" (info recordParser (progDesc "Record/update PR commit snapshot"))
 <> command "update" (info recordParser (progDesc "Synonym for record"))
 <> command "u" (info recordParser (progDesc "Shortcut for update"))
 <> command "r" (info recordParser (progDesc "Shortcut for record"))
 <> command "rec" (info recordParser (progDesc "Shortcut for record"))
 <> command "list" (info (pure List) (progDesc "List tracked PRs"))
  )
  where
    approveParser = Approve
      <$> optional (strArgument (metavar "BRANCH" <> help "Branch to approve (default: current)"))
      <*> strOption (long "by" <> metavar "NAME" <> help "Approver name")
    statusParser = Status
      <$> optional (strArgument (metavar "BRANCH" <> help "Branch to check (default: current)"))
    recordParser = Record
      <$> optional (strArgument (metavar "BRANCH" <> help "Branch to record (default: current)"))

main :: IO ()
main = do
  cmd <- execParser $ info (commandParser <**> helper) idm
  state <- loadState
  case cmd of
    Approve mbBranch by -> do
      branch <- case mbBranch of
        Just b -> return b
        Nothing -> fmap trimTrailing (readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] "")
      let pr = Map.findWithDefault (PRState "open" [] []) branch state
      let newApprovals = if by `notElem` prApprovals pr then by : prApprovals pr else prApprovals pr
      let newPr = pr { prStatus = prStatus pr, prApprovals = newApprovals, prSnapshots = prSnapshots pr }
      let newState = Map.insert branch newPr state
      saveState newState
      putStrLn $ "Approved " ++ branch ++ " by " ++ by
    Status mbBranch -> do
      branch <- case mbBranch of
        Just b -> return b
        Nothing -> fmap trimTrailing (readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] "")
      case Map.lookup branch state of
        Nothing -> putStrLn $ "No status for " ++ branch
        Just pr -> do
          putStrLn $ "Status: " ++ prStatus pr
          putStr "Approvals: "
          putStrLn $ if null (prApprovals pr) then "none" else intercalate ", " (prApprovals pr)
          if null (prSnapshots pr)
            then putStrLn "No snapshots recorded"
            else do
              base <- getBaseBranch
              (code, out, _) <- readProcessWithExitCode "git" ["rev-parse", "--verify", branch] ""
              let branch_exists = code == ExitSuccess
              let branch_tip = if branch_exists then trimTrailing out else ""
              let all_commits = foldl (\m ps -> foldl (\m' ci -> if Map.member (ciHash ci) m' then m' else Map.insert (ciHash ci) (ci, psTimestamp ps) m') m (psCommits ps)) Map.empty (prSnapshots pr)
              temp_list <- mapM (\(ci, ts) -> do
                is_m <- isAncestor (ciHash ci) base
                is_p <- if not branch_exists then return False else isAncestor (ciHash ci) branch_tip
                let s = if is_m then "merged" else if is_p then "pending" else "removed"
                return (ci, s, ts)
                ) (Map.elems all_commits)
              let sorted_statuses = sortBy (\(_,_,ts1) (_,_,ts2) -> compare ts1 ts2) temp_list
              let mergedCount = length [() | (_,s,_) <- sorted_statuses, s == "merged"]
              let pendingCount = length [() | (_,s,_) <- sorted_statuses, s == "pending"]
              let removedCount = length [() | (_,s,_) <- sorted_statuses, s == "removed"]
              let total = length sorted_statuses
              putStrLn $ "Latest snapshot at " ++ psTimestamp (last (prSnapshots pr))
              putStrLn $ "Commit status: " ++ show mergedCount ++ " merged, " ++ show pendingCount ++ " pending, " ++ show removedCount ++ " removed out of " ++ show total
              let latest = last (prSnapshots pr)
              let latest_hashes = map ciHash (psCommits latest)
              let hash_to_s = Map.fromList [ (ciHash ci, s) | (ci, s, _) <- temp_list ]
              let all_latest_merged = not (null latest_hashes) && all (\h -> Map.lookup h hash_to_s == Just "merged") latest_hashes
              if all_latest_merged
                then putStrLn $ "PR is fully merged into " ++ base
                else return ()
              putStrLn "All historical commits:"
              mapM_ (\(ci, s, _) -> putStrLn $ "- " ++ take 7 (ciHash ci) ++ " " ++ ciMessage ci ++ " (" ++ s ++ ")") sorted_statuses
    Record mbBranch -> do
      branch <- case mbBranch of
        Just b -> return b
        Nothing -> fmap trimTrailing (readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] "")
      msg <- recordPR branch
      putStrLn $ "Recorded snapshot for " ++ branch ++ (if null msg then "" else ". " ++ msg)
    List -> do
      if Map.null state
        then putStrLn "No PRs tracked"
        else Map.foldrWithKey (\b pr acc -> putStrLn (b ++ ": " ++ prStatus pr ++ " (approvals: " ++ show (length $ prApprovals pr) ++ ")") >> acc) (return ()) state
