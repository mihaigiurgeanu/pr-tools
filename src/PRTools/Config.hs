{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PRTools.Config where

import Control.Exception (catch, IOException)
import Data.List (drop, init, length, null)
import Data.Maybe (fromMaybe)
import Data.Yaml (FromJSON(..), ParseException, decodeFileEither, parseJSON, withObject, (.:), (.:?))
import System.Directory (doesFileExist)
import System.Process (readProcess)

data Config = Config { cfgBaseBranch :: String, cfgSlackWebhook :: Maybe String, cfgStaleDays :: Maybe Int, cfgSlackToken :: Maybe String, cfgSlackChannel :: Maybe String } deriving Show

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config <$> v .: "base-branch" <*> v .:? "slack-webhook" <*> v .:? "stale-days" <*> v .:? "slack-token" <*> v .:? "slack-channel"

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

getSlackWebhook :: IO (Maybe String)
getSlackWebhook = do
  let configPath = ".pr-tools.yaml"
  exists <- doesFileExist configPath
  if exists
    then do
      res <- decodeFileEither configPath
      case res of
        Right config -> return $ cfgSlackWebhook config
        Left _ -> return Nothing
    else return Nothing

getSlackToken :: IO (Maybe String)
getSlackToken = do
  let configPath = ".pr-tools.yaml"
  exists <- doesFileExist configPath
  if exists
    then do
      res <- decodeFileEither configPath
      case res of
        Right config -> return $ cfgSlackToken config
        Left _ -> return Nothing
    else return Nothing

getSlackChannel :: IO (Maybe String)
getSlackChannel = do
  let configPath = ".pr-tools.yaml"
  exists <- doesFileExist configPath
  if exists
    then do
      res <- decodeFileEither configPath
      case res of
        Right config -> return $ cfgSlackChannel config
        Left _ -> return Nothing
    else return Nothing

getStaleDays :: IO Int
getStaleDays = do
  let configPath = ".pr-tools.yaml"
  exists <- doesFileExist configPath
  if exists
    then do
      res <- decodeFileEither configPath
      case res of
        Right config -> return $ fromMaybe 14 (cfgStaleDays config)
        Left _ -> return 14
    else return 14

getDynamicBase :: IO String
getDynamicBase = do
  ref <- catch (readProcess "git" ["symbolic-ref", "refs/remotes/origin/HEAD"] "")
               (\(_ :: IOException) -> return "")
  let trimmedRef = reverse $ dropWhile (== '\n') $ reverse ref  -- remove trailing newlines
  if null trimmedRef
    then return "main"
    else do
      let prefix :: String = "refs/remotes/origin/"
      let branch = drop (length prefix) trimmedRef
      return $ if null branch then "main" else branch

trimTrailing :: String -> String
trimTrailing str = reverse $ dropWhile (== '\n') $ reverse str

sanitizeBranch :: String -> String
sanitizeBranch = map (\c -> if c == '/' then '-' else c)

reviewDir :: FilePath
reviewDir = ".pr-reviews"
