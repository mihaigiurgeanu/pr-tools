import PRTools.Config (getBaseBranch, trimTrailing, sanitizeBranch)
import PRTools.PRState (recordPR)

import Data.List (head, lines, null, unlines)
import Options.Applicative
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.Environment (getArgs, lookupEnv)
import System.FilePath ((</>))
import System.Process (readProcess, callProcess)

data Opts = Opts
  { optBranch :: Maybe String
  , optBase :: Maybe String
  , optMessage :: Maybe String
  }

optsParser :: Parser Opts
optsParser = Opts
  <$> optional (strArgument (metavar "BRANCH" <> help "The feature branch (default: current branch)"))
  <*> optional (strOption (long "base-branch" <> metavar "BASE" <> help "Override the base branch"))
  <*> optional (strOption (long "message" <> short 'm' <> metavar "MESSAGE" <> help "PR description"))

main :: IO ()
main = do
  opts <- execParser $ info (optsParser <**> helper) idm
  baseB <- case optBase opts of
    Just b -> return b
    Nothing -> getBaseBranch
  branch <- case optBranch opts of
    Just b -> return b
    Nothing -> fmap trimTrailing (readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] "")
  author <- fmap trimTrailing (readProcess "git" ["config", "user.name"] "")
  commitsOut <- readProcess "git" ["log", "--format=%h %s", baseB ++ ".." ++ branch, "--"] ""
  let commitList = unlines $ map ("- " ++) $ lines commitsOut
  diffSummary <- readProcess "git" ["diff", "--stat", baseB, branch, "--"] ""
  let descLines = case optMessage opts of
        Just msg -> lines msg
        Nothing -> ["(Enter your PR description here)"]

  let md = unlines
        ( [ "# PR Snapshot for " ++ branch
          , ""
          , "**Author:** " ++ author
          , ""
          , "## Description"
          , ""
          ] ++ descLines ++
          [ ""
          , "## Commits"
          , ""
          , commitList
          , ""
          , "## Diff Summary"
          , ""
          , "```"
          , diffSummary
          , "```"
          ] )
  root <- fmap trimTrailing (readProcess "git" ["rev-parse", "--show-toplevel"] "")
  let outputDir = root </> ".pr-drafts"
  createDirectoryIfMissing True outputDir
  let safeBranch = sanitizeBranch branch
  let outputPath = outputDir </> (safeBranch ++ ".md")
  writeFile outputPath md
  case optMessage opts of
    Just _ -> return ()
    Nothing -> do
      editorM <- lookupEnv "EDITOR"
      let editor = maybe "vi" id editorM
      callProcess editor [outputPath]
  putStrLn $ "Snapshot written to " ++ outputPath
  recordPR branch
