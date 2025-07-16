import Data.Algorithm.Diff (PolyDiff(..), getGroupedDiff)
import Data.Char (isSpace)
import Data.List (filter, foldl')
import Data.Maybe (fromMaybe)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import Options.Applicative
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.FilePath.Glob (glob)
import System.IO (hPutStrLn, stderr)
import System.IO.Temp (withSystemTempFile)
import System.Process (callProcess, readProcess)
import PRTools.Config (getBaseBranch, reviewDir)
import PRTools.ReviewState

data Global = Global { gBaseBranch :: Maybe String }

globalParser :: Parser Global
globalParser = Global
  <$> optional (strOption
      ( long "base-branch"
     <> metavar "BASE"
     <> help "Override the base branch"
      ))

data Command =
    Start
  | Next
  | Previous
  | Open
  | Files
  | Changes
  | Comment { cFile :: String, cLine :: Int, cText :: String }
  | Resolve { rId :: String }
  | End
  | List

commandParser :: Parser Command
commandParser = subparser
  ( command "start" (info (pure Start) (progDesc "Start review"))
 <> command "next" (info (pure Next) (progDesc "Next file"))
 <> command "previous" (info (pure Previous) (progDesc "Previous file"))
 <> command "open" (info (pure Open) (progDesc "Open current file"))
 <> command "files" (info (pure Files) (progDesc "List files"))
 <> command "changes" (info (pure Changes) (progDesc "Show changes"))
 <> command "comment" (info commentParser (progDesc "Add comment"))
 <> command "resolve" (info resolveParser (progDesc "Resolve comment"))
 <> command "end" (info (pure End) (progDesc "End review"))
 <> command "list" (info (pure List) (progDesc "List reviews"))
  )
  where
    commentParser = Comment
      <$> strOption (long "file" <> metavar "FILE")
      <*> option auto (long "line" <> metavar "LINE")
      <*> strOption (long "text" <> metavar "TEXT")
    resolveParser = Resolve
      <$> strOption (long "id" <> metavar "ID")

getReviewFile :: String -> String -> IO FilePath
getReviewFile branch reviewer = do
  createDirectoryIfMissing False reviewDir
  return $ reviewDir </> branch ++ "-" ++ reviewer ++ ".yaml"

generateConflictContent :: [String] -> [String] -> [String]
generateConflictContent baseLines featureLines =
  let groups = getGroupedDiff baseLines featureLines
  in recBuild [] groups
  where
    recBuild acc [] = acc
    recBuild acc (g:gs) = case g of
      Both ls _ -> recBuild (acc ++ ls) gs
      First ls -> case gs of
        (Second ms : rest) -> recBuild (acc ++ ["<<<<<<< BASE"] ++ ls ++ ["======="] ++ ms ++ [">>>>>>> FEATURE"]) rest
        _ -> recBuild (acc ++ ["<<<<<<< BASE"] ++ ls ++ ["======="] ++ [">>>>>>> FEATURE"]) gs
      Second ms -> recBuild (acc ++ ["<<<<<<< BASE"] ++ ["======="] ++ ms ++ [">>>>>>> FEATURE"]) gs

extractEditedFeature :: [String] -> [String]
extractEditedFeature editedLines =
  let (feat, _, _) = foldl' (\(f, inb, inf) line ->
                                 if line == "<<<<<<< BASE" then (f, True, False)
                                 else if line == "=======" then (f, False, True)
                                 else if line == ">>>>>>> FEATURE" then (f, False, False)
                                 else if inf || (not inb && not inf) then (f ++ [line], inb, inf)
                                 else (f, inb, inf)
                            ) ([], False, False) editedLines
  in feat

extractComments :: [String] -> [String] -> [(Int, String)]
extractComments original edited =
  let diffs = getGroupedDiff original edited
      (cmts, al, current) = foldl' (\(cs, al, cur) d ->
        case d of
          Both _ _ ->
            let newCs = if null cur then cs else cs ++ [(if al - 1 == 0 then 1 else al - 1, unlines cur)]
            in (newCs, al + 1, [])
          First _ ->
            (cs, al + 1, cur)
          Second l -> (cs, al, cur ++ [l])
        ) ([], 1, []) diffs
      finalCmts = if null current then cmts else cmts ++ [(if al == 0 then 1 else al, unlines current)]
  in filter (\(_, t) -> not (all isSpace t)) finalCmts  -- filter non-empty

openEditor :: String -> String -> String -> IO [(Int, String)]
openEditor filePath branch baseB = do
  baseContent <- readProcess "git" ["show", baseB ++ ":" ++ filePath] ""
  featureContent <- readProcess "git" ["show", branch ++ ":" ++ filePath] ""
  let baseLines = lines baseContent
  let featureLines = lines featureContent
  let conflictLines = generateConflictContent baseLines featureLines
  let conflictContent = unlines conflictLines
  withSystemTempFile "review.tmp" $ \tmpPath _ -> do
    writeFile tmpPath conflictContent
    editor <- fromMaybe "vim" <$> lookupEnv "EDITOR"
    callProcess editor [tmpPath]
    editedContent <- readFile tmpPath
    let editedLines = lines editedContent
    let editedFeature = extractEditedFeature editedLines
    return $ extractComments featureLines editedFeature

data App = App { appGlobal :: Global, appCommand :: Command }

appParser :: Parser App
appParser = App <$> globalParser <*> commandParser

main :: IO ()
main = do
  App global cmd <- execParser $ info (appParser <**> helper) idm
  baseB <- case gBaseBranch global of
    Just b -> return b
    Nothing -> getBaseBranch
  branch <- fmap init (readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] "")
  reviewer <- fmap init (readProcess "git" ["config", "user.name"] "")
  reviewFile <- getReviewFile branch reviewer
  case cmd of
    Start -> do
      filesOut <- readProcess "git" ["diff", "--name-only", baseB] ""
      let files = lines filesOut
      let state = ReviewState "active" 0 files [] branch reviewer
      exists <- doesFileExist reviewFile
      if exists then putStrLn "Review already started, resuming" else return ()
      saveReviewState reviewFile state
    Next -> handleNav (\s -> if rsCurrentIndex s < length (rsFiles s) - 1 then s { rsCurrentIndex = rsCurrentIndex s + 1 } else s) reviewFile branch baseB
    Previous -> handleNav (\s -> if rsCurrentIndex s > 0 then s { rsCurrentIndex = rsCurrentIndex s - 1 } else s) reviewFile branch baseB
    Open -> handleNav id reviewFile branch baseB
    Files -> do
      out <- readProcess "git" ["diff", "--name-only", baseB] ""
      putStr out
    Changes -> do
      out <- readProcess "git" ["diff", baseB] ""
      putStr out
    Comment file line text -> do
      mState <- loadReviewState reviewFile
      case mState of
        Nothing -> do
          hPutStrLn stderr "No active review"
          exitFailure
        Just state -> if rsStatus state /= "active" then do
          hPutStrLn stderr "No active review"
          exitFailure
          else do
            u <- nextRandom
            let cmtId = take 8 $ filter (/= '-') $ toString u
            let newComment = Cmt cmtId file line text False
            let newState = state { rsComments = rsComments state ++ [newComment] }
            saveReviewState reviewFile newState
            putStrLn $ "Added comment " ++ cmtId
    Resolve rid -> do
      mState <- loadReviewState reviewFile
      case mState of
        Nothing -> do
          hPutStrLn stderr "No review"
          exitFailure
        Just state -> do
          let updatedComments = map (\c -> if cmId c == rid then c { cmResolved = True } else c) (rsComments state)
          if updatedComments == rsComments state
            then putStrLn "Comment not found"
            else do
              let newState = state { rsComments = updatedComments }
              saveReviewState reviewFile newState
              putStrLn $ "Resolved " ++ rid
    End -> do
      mState <- loadReviewState reviewFile
      case mState of
        Nothing -> do
          hPutStrLn stderr "No review"
          exitFailure
        Just state -> do
          let newState = state { rsStatus = "closed" }
          saveReviewState reviewFile newState
          putStrLn "Review ended"
    List -> do
      rfs <- glob (reviewDir </> "*.yaml")
      mapM_ (\rf -> do
        mState <- loadReviewState rf
        case mState of
          Just state -> putStrLn $ rsBranch state ++ " by " ++ rsReviewer state ++ ": " ++ rsStatus state
          Nothing -> return ()
        ) rfs

handleNav :: (ReviewState -> ReviewState) -> FilePath -> String -> String -> IO ()
handleNav update rf branch baseB = do
  mState <- loadReviewState rf
  case mState of
    Nothing -> do
      hPutStrLn stderr "No active review"
      exitFailure
    Just state -> if rsStatus state /= "active" then do
      hPutStrLn stderr "No active review"
      exitFailure
      else do
        let updatedState = update state
        if rsCurrentIndex updatedState == rsCurrentIndex state && update /= id
          then if rsCurrentIndex state == 0 then putStrLn "No previous files" else putStrLn "No more files"
          else do
            saveReviewState rf updatedState
            let filePath = rsFiles updatedState !! rsCurrentIndex updatedState
            newCmts <- openEditor filePath branch baseB
            let filteredCmts = filter (\(_, t) -> not (all isSpace t)) newCmts
            updatedCmts <- mapM (\(l, t) -> do
              u <- nextRandom
              let cid = take 8 $ filter (/= '-') $ toString u
              return $ Cmt cid filePath l t False
              ) filteredCmts
            let finalState = updatedState { rsComments = rsComments updatedState ++ updatedCmts }
            saveReviewState rf finalState
