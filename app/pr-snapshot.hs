import PRTools.Config (getBaseBranch, trimTrailing)

import Data.List (head, lines, null, unlines)
import Options.Applicative
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.Process (readProcess)

data Opts = Opts
  { optBranch :: Maybe String
  , optBase :: Maybe String
  }

optsParser :: Parser Opts
optsParser = Opts
  <$> optional (strArgument (metavar "BRANCH" <> help "The feature branch (default: current branch)"))
  <*> optional (strOption (long "base-branch" <> metavar "BASE" <> help "Override the base branch"))

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
  let md = unlines
        [ "# PR Snapshot for " ++ branch
        , ""
        , "**Author:** " ++ author
        , ""
        , "## Commits"
        , ""
        , commitList
        , ""
        , "## Diff Summary"
        , ""
        , "```"
        , diffSummary
        , "```"
        ]
  root <- fmap trimTrailing (readProcess "git" ["rev-parse", "--show-toplevel"] "")
  let outputDir = root </> ".pr-drafts"
  createDirectoryIfMissing True outputDir
  let outputPath = outputDir </> (branch ++ ".md")
  writeFile outputPath md
  putStrLn $ "Snapshot written to " ++ outputPath
