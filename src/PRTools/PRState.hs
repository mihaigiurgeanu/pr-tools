module PRTools.PRState where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Aeson.Key (fromString)
import Data.Aeson.Types ((.!=))
import qualified Data.Map.Strict as Map
import Data.Yaml (FromJSON(..), decodeFileEither, encodeFile, parseJSON, withObject, (.:), (.:?))
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

data PRState = PRState { prStatus :: String, prApprovals :: [String] } deriving Show

instance FromJSON PRState where
  parseJSON = withObject "PRState" $ \v -> PRState <$> v .: fromString "status" <*> v .:? fromString "approvals" .!= []

instance ToJSON PRState where
  toJSON p = object [fromString "status" .= prStatus p, fromString "approvals" .= prApprovals p]

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
