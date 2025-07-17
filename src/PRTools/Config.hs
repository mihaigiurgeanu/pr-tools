{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PRTools.Config where

import Control.Exception (catch, IOException)
import Data.List (drop, init, length, null)
import Data.Yaml (FromJSON(..), ParseException, decodeFileEither, parseJSON, withObject, (.:), (.:?))
import System.Directory (doesFileExist)
import System.Process (readProcess)

data Config = Config { cfgBaseBranch :: String, cfgSlackWebhook :: Maybe String } deriving Show

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config <$> v .: "base-branch" <*> v .:? "slack-webhook"

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
