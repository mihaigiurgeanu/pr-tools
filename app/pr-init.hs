import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess)
import PRTools.Config (Config(..), getBaseBranch, getSlackWebhook)

import Control.Monad (when)
import Data.List (isInfixOf)
import System.IO (IOMode(ReadMode, AppendMode), hGetContents, hPutStr, withFile)

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
  if configExists
    then do
      mbBase <- getBaseBranch
      mbWebhook <- getSlackWebhook
      putStrLn $ "Current base-branch: " ++ mbBase
      case mbWebhook of
        Just wh -> putStrLn $ "Current slack-webhook: " ++ wh
        Nothing -> putStrLn "slack-webhook not set."
    else do
      putStrLn "Creating .pr-tools.yaml"
      putStr "Enter base-branch (default: main): "
      baseInput <- getLine
      let base = if null baseInput then "main" else baseInput
      putStr "Enter slack-webhook (optional, press Enter to skip): "
      webhookInput <- getLine
      let webhook = if null webhookInput then "" else "slack-webhook: " ++ webhookInput
      let content = "base-branch: " ++ base ++ "\n" ++ webhook
      writeFile configPath content
      putStrLn ".pr-tools.yaml created."
