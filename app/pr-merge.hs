import Control.Monad (when)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.List (head, length, null, (!!))
import Data.Time (formatTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale)
import Network.HTTP.Client (RequestBody(RequestBodyLBS), httpLbs, method, newManager, parseRequest, requestBody, requestHeaders, responseStatus)
import Network.HTTP.Client.Tls (tlsManagerSettings)
import Network.HTTP.Types (statusCode)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (IOMode(AppendMode), hPutStrLn, stderr, withFile)
import System.Process (callProcess, readProcess)
import PRTools.Config (baseBranch)
import PRTools.PRState

main :: IO ()
main = do
  args <- getArgs
  when (null args) $ do
    hPutStrLn stderr "Usage: pr-merge <branch> [--strategy <fast-forward|squash|rebase>]"
    exitFailure
  let branch = head args
  let strategy = if length args > 2 && args !! 1 == "--strategy" then args !! 2 else "fast-forward"
  state <- loadState
  case Map.lookup branch state of
    Nothing -> do
      hPutStrLn stderr $ "PR " ++ branch ++ " not approved or not tracked"
      exitFailure
    Just pr -> if null (prApprovals pr) then do
      hPutStrLn stderr $ "PR " ++ branch ++ " not approved or not tracked"
      exitFailure
      else do
        callProcess "git" ["checkout", baseBranch]
        case strategy of
          "fast-forward" -> callProcess "git" ["merge", "--ff-only", branch]
          "squash" -> do
            callProcess "git" ["merge", "--squash", branch]
            callProcess "git" ["commit", "--message", "Squashed merge of " ++ branch]
          "rebase" -> do
            callProcess "git" ["checkout", branch]
            callProcess "git" ["rebase", baseBranch]
            callProcess "git" ["checkout", baseBranch]
            callProcess "git" ["merge", "--ff-only", branch]
          _ -> do
            hPutStrLn stderr "Invalid strategy"
            exitFailure
        let newState = Map.insert branch (PRState "merged" (prApprovals pr)) state
        saveState newState
        currentTime <- getCurrentTime
        let dateStr = formatTime defaultTimeLocale "%Y-%m-%d" currentTime
        withFile "CHANGELOG.md" AppendMode $ \h -> hPutStrLn h $ "\n- Merged " ++ branch ++ " using " ++ strategy ++ " on " ++ dateStr
        mbWebhook <- lookupEnv "SLACK_WEBHOOK"
        case mbWebhook of
          Nothing -> return ()
          Just webhook -> do
            manager <- newManager tlsManagerSettings
            initReq <- parseRequest webhook
            let req = initReq
                  { method = "POST"
                  , requestBody = RequestBodyLBS $ encode $ object ["text" .= ("PR " ++ branch ++ " merged using " ++ strategy)]
                  , requestHeaders = [("Content-Type", "application/json")]
                  }
            response <- httpLbs req manager
            return ()
        putStrLn $ "Merged " ++ branch ++ " using " ++ strategy
