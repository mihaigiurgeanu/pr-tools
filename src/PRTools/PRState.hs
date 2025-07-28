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
import System.Exit (ExitCode(..))

data CommitInfo = CommitInfo { ciHash :: String, ciMessage :: String } deriving (Eq, Show)

instance FromJSON CommitInfo where
  parseJSON = withObject "CommitInfo" $ \v -> CommitInfo
    <$> v .: fromString "hash"
    <*> v .: fromString "message"

instance ToJSON CommitInfo where
  toJSON ci = object
    [ fromString "hash" .= ciHash ci
    , fromString "message" .= ciMessage ci
    ]

data PRSnapshot = PRSnapshot { psTimestamp :: String, psCommits :: [CommitInfo] } deriving (Eq, Show)

instance FromJSON PRSnapshot where
  parseJSON = withObject "PRSnapshot" $ \v -> PRSnapshot
    <$> v .: fromString "timestamp"
    <*> v .: fromString "commits"

instance ToJSON PRSnapshot where
  toJSON ps = object
    [ fromString "timestamp" .= psTimestamp ps
    , fromString "commits" .= psCommits ps
    ]

data PRState = PRState { prStatus :: String, prApprovals :: [String], prSnapshots :: [PRSnapshot] } deriving (Eq, Show)

instance FromJSON PRState where
  parseJSON = withObject "PRState" $ \v -> PRState
    <$> v .: fromString "status"
    <*> v .:? fromString "approvals" .!= []
    <*> v .:? fromString "snapshots" .!= []

instance ToJSON PRState where
  toJSON p = object
    [ fromString "status" .= prStatus p
    , fromString "approvals" .= prApprovals p
    , fromString "snapshots" .= prSnapshots p
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

recordPR :: String -> IO String
recordPR branch = do
  state <- loadState
  let existing = Map.lookup branch state
  base <- getBaseBranch
  (branchExists, commits) <- do
    (code, out, _) <- readProcessWithExitCode "git" ["rev-parse", "--verify", branch] ""
    if code == ExitSuccess
      then do
        logOut <- readProcess "git" ["log", "--format=%H %s", base ++ ".." ++ branch] ""
        let commitLines = lines logOut
        let cs = map (\ln -> let h = take 40 ln
                                 m = drop 41 ln
                             in CommitInfo h m) (filter (not . null) commitLines)
        return (True, cs)
      else return (False, [])

  case existing of
    Nothing -> do
      if not branchExists
        then do
          hPutStrLn stderr $ "Branch " ++ branch ++ " does not exist and is not tracked."
          exitFailure
        else do
          -- Create new entry and proceed
          let ex = PRState "open" [] []
          currentTime <- getCurrentTime
          let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
          let newSnapshot = PRSnapshot timeStr commits
          let updatedSnapshots = prSnapshots ex ++ [newSnapshot]

          -- Stale check (use latest snapshot even if no new one)
          staleDays <- getStaleDays
          let snapsForStale = if null updatedSnapshots then prSnapshots ex else updatedSnapshots
          let isStale = case snapsForStale of
                          [] -> False
                          snaps -> let lastSnap = last snaps
                                       lastTimeM = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (psTimestamp lastSnap)
                                   in case lastTimeM of
                                        Just lastTime -> diffUTCTime currentTime lastTime > fromIntegral (staleDays * 86400) && null commits
                                        Nothing -> False
          let statusAfterStale = if isStale && prStatus ex /= "stale" && prStatus ex /= "merged" then "stale" else prStatus ex

          -- Merged check (use latest commits if branch missing)
          let checkCommits = commits
          allMerged <- if null checkCommits then return False else and <$> mapM (\ci -> isAncestor (ciHash ci) base) checkCommits
          let finalStatus = if allMerged && statusAfterStale /= "merged" then "merged" else statusAfterStale

          let updated = ex { prSnapshots = updatedSnapshots, prStatus = finalStatus }
          let newState = Map.insert branch updated state
          saveState newState
          let msg = if finalStatus /= prStatus ex then "Status updated to " ++ finalStatus else ""
          return $ msg ++ " (new tracking started)"
    Just ex -> do
      currentTime <- getCurrentTime
      let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
      let updatedSnapshots = if null commits
                             then prSnapshots ex
                             else prSnapshots ex ++ [PRSnapshot timeStr commits]

      -- Stale check (use latest snapshot even if no new one)
      staleDays <- getStaleDays
      let snapsForStale = if null updatedSnapshots then prSnapshots ex else updatedSnapshots
      let isStale = case snapsForStale of
                      [] -> False
                      snaps -> let lastSnap = last snaps
                                   lastTimeM = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (psTimestamp lastSnap)
                               in case lastTimeM of
                                    Just lastTime -> diffUTCTime currentTime lastTime > fromIntegral (staleDays * 86400) && null commits
                                    Nothing -> False
      let statusAfterStale = if isStale && prStatus ex /= "stale" && prStatus ex /= "merged" then "stale" else prStatus ex

      -- Merged check (use latest commits if branch missing)
      let checkCommits = if not (null commits) then commits else if null (prSnapshots ex) then [] else psCommits (last (prSnapshots ex))
      allMerged <- if null checkCommits then return False else and <$> mapM (\ci -> isAncestor (ciHash ci) base) checkCommits
      let finalStatus = if allMerged && statusAfterStale /= "merged" then "merged" else statusAfterStale

      let updated = ex { prSnapshots = updatedSnapshots, prStatus = finalStatus }
      let newState = Map.insert branch updated state
      saveState newState
      let msg = if finalStatus /= prStatus ex
                then "Status updated to " ++ finalStatus
                else if null commits
                     then "No update performed"
                     else ""
      let extraMsg = if not branchExists then " (branch does not exist; using latest snapshot for checks)" else ""
      return $ msg ++ extraMsg
