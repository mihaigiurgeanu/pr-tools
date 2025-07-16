import Control.Monad (when)
import qualified Data.Map.Strict as Map
import Data.List (head, intercalate, length, notElem, null, (!!))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess)
import PRTools.PRState

main :: IO ()
main = do
  args <- getArgs
  when (null args) $ do
    hPutStrLn stderr "Usage: pr-track <command> [args]"
    exitFailure
  let command = head args
  state <- loadState
  case command of
    "approve" -> do
      when (length args < 2) $ do
        hPutStrLn stderr "Usage: pr-track approve <branch> [--by <name>]"
        exitFailure
      let branch = args !! 1
      by <- fmap init (readProcess "git" ["config", "user.name"] "")
      let by' = if length args > 3 && args !! 2 == "--by" then args !! 3 else by
      let pr = Map.findWithDefault (PRState "open" []) branch state
      let newApprovals = if by' `notElem` prApprovals pr then by' : prApprovals pr else prApprovals pr
      let newPr = PRState (prStatus pr) newApprovals
      let newState = Map.insert branch newPr state
      saveState newState
      putStrLn $ "Approved " ++ branch ++ " by " ++ by'
    "status" -> do
      when (length args < 2) $ do
        hPutStrLn stderr "Usage: pr-track status <branch>"
        exitFailure
      let branch = args !! 1
      case Map.lookup branch state of
        Nothing -> putStrLn $ "No status for " ++ branch
        Just pr -> do
          putStrLn $ "Status: " ++ prStatus pr
          putStr "Approvals: "
          putStrLn $ if null (prApprovals pr) then "none" else intercalate ", " (prApprovals pr)
    "list" -> do
      if Map.null state
        then putStrLn "No PRs tracked"
        else Map.foldrWithKey (\b pr _ -> putStrLn $ b ++ ": " ++ prStatus pr ++ " (approvals: " ++ show (length $ prApprovals pr) ++ ")") () state
    _ -> do
      hPutStrLn stderr "Unknown command"
      exitFailure
