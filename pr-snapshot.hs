import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.Process (readProcess)

main :: IO ()
main = do
  args <- getArgs
  branch <- if not (null args) then return (head args) else fmap init (readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] "")
  author <- fmap init (readProcess "git" ["config", "user.name"] "")
  let base = "main"
  commitsOut <- readProcess "git" ["log", "--format=%h %s", base ++ ".." ++ branch] ""
  let commitList = unlines $ map ("- " ++) $ lines commitsOut
  diffSummary <- readProcess "git" ["diff", "--stat", base, branch] ""
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
  home <- getHomeDirectory
  let outputDir = home </> "pr-drafts"
  createDirectoryIfMissing True outputDir
  let outputPath = outputDir </> (branch ++ ".md")
  writeFile outputPath md
  putStrLn $ "Snapshot written to " ++ outputPath
