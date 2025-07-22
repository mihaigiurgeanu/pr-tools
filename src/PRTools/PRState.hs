module PRTools.PRState where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Aeson.Key (fromString)
import Data.Aeson.Types ((.!=))
import qualified Data.Map.Strict as Map
import Data.Yaml (FromJSON(..), decodeFileEither, encodeFile, parseJSON, withObject, (.:), (.:?))
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Data.Time (formatTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale)
import System.Process (readProcess)
import PRTools.Config (getBaseBranch, trimTrailing)

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

recordPR :: String -> IO ()
recordPR branch = do
  state <- loadState
  base <- getBaseBranch
  logOut <- readProcess "git" ["log", "--format=%H %s", base ++ ".." ++ branch] ""
  let commitLines = lines logOut
  let commits = map (\ln -> let h = take 40 ln
                                m = drop 41 ln
                           in CommitInfo h m) (filter (not . null) commitLines)
  currentTime <- getCurrentTime
  let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
  let newSnapshot = PRSnapshot timeStr commits
  let existing = Map.findWithDefault (PRState "open" [] []) branch state
  let updated = existing { prSnapshots = prSnapshots existing ++ [newSnapshot] }
  let newState = Map.insert branch updated state
  saveState newState
