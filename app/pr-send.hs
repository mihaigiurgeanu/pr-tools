{-# LANGUAGE OverloadedStrings #-}

import PRTools.Config (trimTrailing)

import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive (mk)
import Network.HTTP.Client (RequestBody(RequestBodyLBS), httpLbs, method, newManager, parseRequest, requestBody, requestHeaders, responseStatus)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (statusCode)
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitWith, ExitCode(..))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess)

main :: IO ()
main = do
  mbWebhook <- lookupEnv "SLACK_WEBHOOK"
  case mbWebhook of
    Nothing -> do
      hPutStrLn stderr "Error: SLACK_WEBHOOK environment variable not set"
      exitWith (ExitFailure 1)
    Just webhook -> do
      args <- getArgs
      branch <- if not (null args) then return (head args) else fmap trimTrailing (readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] "")
      home <- getHomeDirectory
      let mdPath = home </> "pr-drafts" </> (branch ++ ".md")
      exists <- doesFileExist mdPath
      if not exists
        then do
          hPutStrLn stderr $ "Error: Snapshot not found at " ++ mdPath
          exitWith (ExitFailure 1)
        else do
          md <- readFile mdPath
          let message = "New PR: " ++ branch ++ "\n\n" ++ md ++ "\n\nReact with :thumbsup: to approve or :thumbsdown: to request changes."
          manager <- newManager tlsManagerSettings
          initReq <- parseRequest webhook
          let req = initReq
                { method = "POST"
                , requestBody = RequestBodyLBS $ encode $ object ["text" .= message]
                , requestHeaders = [(mk "Content-Type", "application/json")]
                }
          response <- httpLbs req manager
          if statusCode (responseStatus response) == 200
            then putStrLn "PR sent to Slack"
            else putStrLn "Error posting to Slack"
