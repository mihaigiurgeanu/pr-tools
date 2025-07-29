import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess)
import PRTools.Config (Config(..), getBaseBranch, getSlackWebhook, getSlackToken, getSlackChannel, getStaleDays)

import Control.Monad (when)
import Data.List (isInfixOf)
import System.IO (IOMode(ReadMode, AppendMode), hGetContents, hPutStr, withFile, hFlush, stdout)

main :: IO ()
main = do
  -- Handle .gitignore
  gitignorePath <- return ".gitignore"
  gitignoreExists <- doesFileExist gitignorePath
  let recommended = [ ".pr-drafts/"
                    , ".pr-reviews/"
                    , ".pr-fixes/"
                    , ".pr-state.yaml"
                    , ".pr-tools.yaml"
                    ]
  if gitignoreExists
    then do
      content <- readFile gitignorePath
      let linesC = lines content
      let missing = filter (\e -> not (any (== e) linesC)) recommended
      if null missing
        then putStrLn ".gitignore already has all recommended entries."
        else do
          putStrLn $ "Adding missing entries to .gitignore: " ++ unwords missing
          withFile gitignorePath AppendMode $ \h -> mapM_ (hPutStrLn h) missing
    else do
      putStrLn "Creating .gitignore with recommended entries."
      writeFile gitignorePath (unlines recommended)

  -- Handle .pr-tools.yaml
  configPath <- return ".pr-tools.yaml"
  configExists <- doesFileExist configPath
  currentBase <- getBaseBranch
  currentWebhook <- getSlackWebhook
  currentToken <- getSlackToken
  currentChannel <- getSlackChannel
  currentStale <- getStaleDays
  let action = if configExists then "Updating" else "Creating"
  putStrLn $ action ++ " .pr-tools.yaml"
  putStr $ "Enter base-branch (current/default: " ++ currentBase ++ "): "
  hFlush stdout
  baseInput <- getLine
  let base = if null baseInput then currentBase else baseInput
  putStr $ "Enter slack-webhook (current: " ++ fromMaybe "not set" currentWebhook ++ ", press Enter to keep): "
  hFlush stdout
  webhookInput <- getLine
  let webhook = if null webhookInput then fromMaybe "" currentWebhook else webhookInput
  let webhookLine = if null webhook then "" else "slack-webhook: " ++ webhook ++ "\n"
  putStr $ "Enter slack-token (current: " ++ fromMaybe "not set" currentToken ++ ", press Enter to keep): "
  hFlush stdout
  tokenInput <- getLine
  let token = if null tokenInput then fromMaybe "" currentToken else tokenInput
  let tokenLine = if null token then "" else "slack-token: " ++ token ++ "\n"
  putStr $ "Enter slack-channel (current: " ++ fromMaybe "not set" currentChannel ++ ", press Enter to keep): "
  hFlush stdout
  channelInput <- getLine
  let channel = if null channelInput then fromMaybe "" currentChannel else channelInput
  let channelLine = if null channel then "" else "slack-channel: " ++ channel ++ "\n"
  putStr $ "Enter stale-days (current/default: " ++ show currentStale ++ ", press Enter to keep): "
  hFlush stdout
  staleInput <- getLine
  let stale = if null staleInput then show currentStale else staleInput
  let staleLine = "stale-days: " ++ stale ++ "\n"
  let content = "base-branch: " ++ base ++ "\n" ++ webhookLine ++ tokenLine ++ channelLine ++ staleLine
  writeFile configPath content
  putStrLn $ ".pr-tools.yaml " ++ (if configExists then "updated." else "created.")
