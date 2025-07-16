import Data.Aeson (ToJSON(..), object, (.=))
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Yaml (FromJSON(..), ToJSON, decodeFileEither, encodeFile, parseJSON, withObject, (.:), (.:?))
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess)

data PRState = PRState { prStatus :: String, prApprovals :: [String] } deriving Show

instance FromJSON PRState where
  parseJSON = withObject "PRState" $ \v -> PRState <$> v .: "status" <*> v .:? "approvals" .!= []

instance ToJSON PRState where
  toJSON p = object ["status" .= prStatus p, "approvals" .= prApprovals p]

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

main :: IO ()
main = do
  args <- getArgs
  if null args
    then do
      hPutStrLn stderr "Usage: pr-track <command> [args]"
      exitFailure
  let command = head args
  state <- loadState
  case command of
    "approve" -> do
      if length args < 2
        then do
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
      if length args < 2
        then do
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
