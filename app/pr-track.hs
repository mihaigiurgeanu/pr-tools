import Control.Monad (when)
import qualified Data.Map.Strict as Map
import Data.List (head, intercalate, length, notElem, null, (!!))
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
              let latest = last (prSnapshots pr)
              putStrLn $ "Latest snapshot at " ++ psTimestamp latest
              base <- getBaseBranch
              statuses <- mapM (\ci -> do
                                 isA <- isAncestor (ciHash ci) base
                                 return (ci, if isA then "merged" else "pending")
                               ) (psCommits latest)
              let mergedCount = length [() | (_, s) <- statuses, s == "merged"]
              let total = length (psCommits latest)
              putStrLn $ "Commit status: " ++ show mergedCount ++ "/" ++ show total ++ " merged"
              if mergedCount == total && total > 0
                then putStrLn $ "PR is fully merged into " ++ base
                else return ()
              mapM_ (\(ci, s) -> putStrLn $ "- " ++ take 7 (ciHash ci) ++ " " ++ ciMessage ci ++ " (" ++ s ++ ")") statuses
    Record mbBranch -> do
      branch <- case mbBranch of
        Just b -> return b
        Nothing -> fmap trimTrailing (readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] "")
      recordPR branch
      putStrLn $ "Recorded snapshot for " ++ branch
    List -> do
      if Map.null state
        then putStrLn "No PRs tracked"
        else Map.foldrWithKey (\b pr acc -> putStrLn (b ++ ": " ++ prStatus pr ++ " (approvals: " ++ show (length $ prApprovals pr) ++ ")") >> acc) (return ()) state

isAncestor :: String -> String -> IO Bool
isAncestor commit branch = do
  (code, _, _) <- readProcessWithExitCode "git" ["merge-base", "--is-ancestor", commit, branch] ""
  return (code == ExitSuccess)
