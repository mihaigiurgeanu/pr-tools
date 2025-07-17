import Control.Monad (when)
import qualified Data.Map.Strict as Map
import Data.List (head, intercalate, length, notElem, null, (!!))
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess)
import PRTools.Config (trimTrailing)
import PRTools.PRState

data Command =
    Approve { aBranch :: Maybe String, aBy :: Maybe String }
  | Status { sBranch :: Maybe String }
  | List

commandParser :: Parser Command
commandParser = subparser
  ( command "approve" (info approveParser (progDesc "Approve a PR"))
 <> command "status" (info statusParser (progDesc "Get PR status"))
 <> command "list" (info (pure List) (progDesc "List tracked PRs"))
  )
  where
    approveParser = Approve
      <$> optional (strArgument (metavar "BRANCH" <> help "Branch to approve (default: current)"))
      <*> optional (strOption (long "by" <> metavar "NAME" <> help "Approver name (default: git user.name)"))
    statusParser = Status
      <$> optional (strArgument (metavar "BRANCH" <> help "Branch to check (default: current)"))

main :: IO ()
main = do
  cmd <- execParser $ info (commandParser <**> helper) idm
  state <- loadState
  case cmd of
    Approve mbBranch mbBy -> do
      branch <- case mbBranch of
        Just b -> return b
        Nothing -> fmap trimTrailing (readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] "")
      by <- case mbBy of
        Just n -> return n
        Nothing -> fmap trimTrailing (readProcess "git" ["config", "user.name"] "")
      let pr = Map.findWithDefault (PRState "open" []) branch state
      let newApprovals = if by `notElem` prApprovals pr then by : prApprovals pr else prApprovals pr
      let newPr = PRState (prStatus pr) newApprovals
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
    List -> do
      if Map.null state
        then putStrLn "No PRs tracked"
        else Map.foldrWithKey (\b pr acc -> putStrLn (b ++ ": " ++ prStatus pr ++ " (approvals: " ++ show (length $ prApprovals pr) ++ ")") >> acc) (return ()) state
