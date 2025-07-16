{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PRTools.Config where

import Control.Exception (catch, IOException)
import Data.List (drop, init, length, null)
import Data.Yaml (FromJSON(..), ParseException, decodeFileEither, parseJSON, withObject, (.:))
import System.Directory (doesFileExist)
import System.Process (readProcess)

data Config = Config { cfgBaseBranch :: String } deriving Show

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config <$> v .: "base-branch"

getBaseBranch :: IO String
getBaseBranch = do
  let configPath = ".pr-tools.yaml"
  exists <- doesFileExist configPath
  if exists
    then do
      res <- decodeFileEither configPath
      case res of
        Right config -> return $ cfgBaseBranch config
        Left _ -> getDynamicBase
    else getDynamicBase

getDynamicBase :: IO String
getDynamicBase = do
  ref <- catch (readProcess "git" ["symbolic-ref", "refs/remotes/origin/HEAD"] "")
               (\(_ :: IOException) -> return "")
  let trimmedRef = init ref  -- remove trailing newline
  if null trimmedRef
    then return "main"
    else do
      let prefix = "refs/remotes/origin/"
      let branch = drop (length prefix) trimmedRef
      return $ if null branch then "main" else branch

reviewDir :: FilePath
reviewDir = ".pr-reviews"
