{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive (mk)
import qualified Data.Map.Strict as Map
import Data.List (head, length, null, (!!))
import Data.Time (formatTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale)
import Network.HTTP.Client (RequestBody(RequestBodyLBS), httpLbs, method, newManager, parseRequest, requestBody, requestHeaders, responseStatus)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (statusCode)
import Options.Applicative
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (IOMode(AppendMode), hPutStrLn, stderr, withFile)
import System.Process (callProcess, readProcess)
import PRTools.Config (getBaseBranch, getSlackWebhook, trimTrailing)
import PRTools.PRState

data Opts = Opts
  { optBranch :: Maybe String
  , optStrategy :: String
  , optBase :: Maybe String
  }

optsParser :: Parser Opts
optsParser = Opts
  <$> optional (strArgument (metavar "BRANCH" <> help "The feature branch to merge (default: current branch)"))
  <*> strOption (long "strategy" <> value "fast-forward" <> showDefault <> metavar "STRATEGY" <> help "Merge strategy (fast-forward, squash, rebase)")
  <*> optional (strOption (long "base-branch" <> metavar "BASE" <> help "Override the base branch"))

main :: IO ()
main = do
  args <- getArgs
  if not (null args) && head args == "help" then putStrLn helpText else do
    opts <- execParser $ info (optsParser <**> helper) idm
    baseB <- case optBase opts of
      Just b -> return b
      Nothing -> getBaseBranch
    branch <- case optBranch opts of
      Just b -> return b
      Nothing -> fmap trimTrailing (readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] "")
    let strategy = optStrategy opts
    state <- loadState
    case Map.lookup branch state of
      Nothing -> do
        hPutStrLn stderr $ "PR " ++ branch ++ " not approved or not tracked"
        exitFailure
      Just pr -> if null (prApprovals pr) then do
        hPutStrLn stderr $ "PR " ++ branch ++ " not approved or not tracked"
        exitFailure
        else do
          callProcess "git" ["checkout", baseB]
          case strategy of
            "fast-forward" -> callProcess "git" ["merge", "--ff-only", branch]
            "squash" -> do
              callProcess "git" ["merge", "--squash", branch]
              callProcess "git" ["commit", "--message", "Squashed merge of " ++ branch]
            "rebase" -> do
              callProcess "git" ["checkout", branch]
              callProcess "git" ["rebase", baseB]
              callProcess "git" ["checkout", baseB]
              callProcess "git" ["merge", "--ff-only", branch]
            _ -> do
              hPutStrLn stderr "Invalid strategy"
              exitFailure
          let newState = Map.insert branch (PRState "merged" (prApprovals pr) (prSnapshots pr)) state
          saveState newState
          currentTime <- getCurrentTime
          let dateStr = formatTime defaultTimeLocale "%Y-%m-%d" currentTime
          withFile "CHANGELOG.md" AppendMode $ \h -> hPutStrLn h $ "\n- Merged " ++ branch ++ " using " ++ strategy ++ " on " ++ dateStr
          mbWebhook <- getSlackWebhook
          case mbWebhook of
            Nothing -> return ()
            Just webhook -> do
              manager <- newManager tlsManagerSettings
              initReq <- parseRequest webhook
              let req = initReq
                    { method = "POST"
                    , requestBody = RequestBodyLBS $ encode $ object ["text" .= ("PR " ++ branch ++ " merged using " ++ strategy)]
                    , requestHeaders = [(mk "Content-Type", "application/json")]
                    }
              response <- httpLbs req manager
              return ()
          putStrLn $ "Merged " ++ branch ++ " using " ++ strategy

helpText :: String
helpText = unlines
  [ "pr-merge"
  , ""
  , "Merge approved PRs with various strategies and update changelog."
  , ""
  , "Usage: pr-merge [BRANCH] [--strategy STRATEGY] [--base-branch BASE]"
  , ""
  , "Arguments:"
  , "  BRANCH              The feature branch to merge (default: current)"
  , ""
  , "Options:"
  , "  --strategy STRATEGY Merge strategy (fast-forward, squash, rebase) (default: fast-forward)"
  , "  --base-branch BASE  Override the base branch"
  , ""
  , "Examples:"
  , "  pr-merge my-feature --strategy squash"
  ]
