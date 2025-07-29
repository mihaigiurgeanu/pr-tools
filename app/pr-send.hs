{-# LANGUAGE OverloadedStrings #-}

import PRTools.Config (trimTrailing, sanitizeBranch, getSlackWebhook, getSlackToken, getSlackChannel)

import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive (mk)
import Network.HTTP.Client (RequestBody(RequestBodyLBS), httpLbs, method, newManager, parseRequest, requestBody, requestHeaders, responseStatus)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (statusCode)
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess)
import PRTools.Slack (sendViaApi, sendViaWebhook)

main :: IO ()
main = do
  args <- getArgs
  branch <- if not (null args) then return (head args) else fmap trimTrailing (readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] "")
  root <- fmap trimTrailing (readProcess "git" ["rev-parse", "--show-toplevel"] "")
  let safeBranch = sanitizeBranch branch
  let mdPath = root </> ".pr-drafts" </> (safeBranch ++ ".md")
  exists <- doesFileExist mdPath
  if not exists
    then do
      hPutStrLn stderr $ "Error: Snapshot not found at " ++ mdPath
      exitWith (ExitFailure 1)
    else do
      md <- readFile mdPath
      let summary = "New PR: " ++ branch ++ ". See attached for details.\n\nReact with :thumbsup: to approve or :thumbsdown: to request changes."
      let fileContent = md
      let filename = "pr-snapshot-" ++ safeBranch ++ ".md"
      mbToken <- getSlackToken
      mbChannel <- getSlackChannel
      mbWebhook <- getSlackWebhook
      case (mbToken, mbChannel) of
        (Just token, Just channel) -> sendViaApi summary fileContent filename channel token
        _ -> case mbWebhook of
          Nothing -> do
            hPutStrLn stderr "Slack not configured"
            exitWith (ExitFailure 1)
          Just webhook -> do
            let message = "New PR: " ++ branch ++ "\n\n" ++ md ++ "\n\nReact with :thumbsup: to approve or :thumbsdown: to request changes."
            sendViaWebhook webhook message
