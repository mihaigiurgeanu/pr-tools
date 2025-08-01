{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (catch, IOException)
import Control.Monad (unless)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.Algorithm.Diff (PolyDiff(..), getGroupedDiff)
import Data.Char (isSpace)
import Data.List (any, filter, findIndex, foldl', intercalate, zipWith, isPrefixOf)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, catMaybes, listToMaybe)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import Network.HTTP.Client (Response, RequestBody(RequestBodyLBS), httpLbs, method, newManager, parseRequest, requestBody, requestHeaders, responseStatus)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (statusCode)
import Options.Applicative
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.FilePath.Glob (glob)
import System.IO (hClose, hPutStr, hPutStrLn, stderr)
import System.IO.Temp (withSystemTempFile)
import System.Process (callProcess, readProcess)
import PRTools.Config (getBaseBranch, getSlackWebhook, reviewDir, trimTrailing, sanitizeBranch, getSlackToken, getSlackChannel)
import PRTools.ReviewState
import PRTools.CommentRenderer
import PRTools.CommentFormatter
import PRTools.PRState (recordPR)
import PRTools.Slack (sendViaApi, sendViaWebhook)

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
  | Open (Maybe String)
  | Files
  | Changes
  | Comment { cFile :: String, cLine :: Int, cText :: String }
  | Resolve { rId :: String, rStatus :: Maybe String, rAnswer :: Maybe String }
  | End
  | List
  | Send
  | Comments Bool Bool Bool
  | ImportAnswers

data NavAction = NavNext | NavPrevious | NavOpen

commandParser :: Parser Command
commandParser = subparser
  ( command "start" (info (pure Start <**> helper) (progDesc "Start review"))
 <> command "next" (info (pure Next <**> helper) (progDesc "Next file"))
 <> command "previous" (info (pure Previous <**> helper) (progDesc "Previous file"))
 <> command "open" (info (Open <$> optional (strOption (long "file" <> metavar "FILE" <> help "Specific file to open")) <**> helper) (progDesc "Open current or specific file"))
 <> command "files" (info (pure Files <**> helper) (progDesc "List files"))
 <> command "changes" (info (pure Changes <**> helper) (progDesc "Show changes"))
 <> command "comment" (info (commentParser <**> helper) (progDesc "Add comment"))
 <> command "resolve" (info (resolveParser <**> helper) (progDesc "Resolve comment"))
 <> command "end" (info (pure End <**> helper) (progDesc "End review"))
 <> command "list" (info (pure List <**> helper) (progDesc "List reviews"))
 <> command "send" (info (pure Send <**> helper) (progDesc "Send review to Slack"))
 <> command "comments" (info (commentsParser <**> helper) (progDesc "List all comments (compact by default)"))
 <> command "import-answers" (info (pure ImportAnswers <**> helper) (progDesc "Import answers from fix summary"))
  )
  where
    commentParser = Comment
      <$> strOption (long "file" <> metavar "FILE")
      <*> option auto (long "line" <> metavar "LINE")
      <*> strOption (long "text" <> metavar "TEXT")
    resolveParser = Resolve
      <$> strOption (long "id" <> metavar "ID")
      <*> optional (strOption (long "status" <> metavar "STATUS" <> help "Optional status (e.g., solved, not-solved, will-not-solve)"))
      <*> optional (strOption (long "answer" <> metavar "ANSWER" <> help "Optional answer/explanation"))
    commentsParser = Comments
      <$> switch (long "with-context" <> help "Display comments with context")
      <*> switch (long "all" <> help "Display all comments (default: unresolved only)")
      <*> switch (long "resolved" <> help "Display only resolved comments")

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- No local parsePastedMessage; use shared

getReviewFile :: String -> String -> IO FilePath
getReviewFile branch reviewer = do
  createDirectoryIfMissing False reviewDir
  let safeBranch = sanitizeBranch branch
  return $ reviewDir </> safeBranch ++ "-" ++ reviewer ++ ".yaml"

openEditor :: String -> String -> String -> [Cmt] -> IO [Cmt]
openEditor filePath branch mergeBase existingCmts = do
  conflictContent <- renderForReview mergeBase branch filePath existingCmts
  withSystemTempFile "review.tmp" $ \tmpPath handle -> do
    hPutStr handle conflictContent
    hClose handle
    editor <- fromMaybe "vim" <$> lookupEnv "EDITOR"
    callProcess editor [tmpPath]
    editedContent <- readFile tmpPath
    let editedLines = lines editedContent
    let editedFeature = extractEditedFeature editedLines  -- Reuse existing extraction
    featureContent <- readProcess "git" ["show", branch ++ ":" ++ filePath] ""
    let featureLines = lines featureContent
    let newPairs = extractComments featureLines editedFeature  -- Reuse
    currentRev <- trimTrailing <$> readProcess "git" ["rev-parse", "HEAD"] ""
    mapM (\(l, t) -> do
      u <- nextRandom
      let cid = take 8 $ filter (/= '-') $ toString u
      return $ Cmt cid filePath l (normalizeComment t) False "not-solved" Nothing currentRev
      ) newPairs

-- Reused helpers
extractEditedFeature :: [String] -> [String]
extractEditedFeature editedLines = go editedLines [] False False
  where
    go [] acc _ _ = reverse acc
    go (ln:lns) acc inBase inFeature
      | ln == "<<<<<<< BASE" = go lns acc True False
      | ln == "=======" = go lns acc False True
      | ln == ">>>>>>> FEATURE" = go lns acc False False
      | "-- REVIEW COMMENT BEGIN [" `isPrefixOf` ln =
          let (block, rest) = span (\l -> not ("-- REVIEW COMMENT END" `isPrefixOf` l)) lns
              skipEnd = if not (null rest) && "-- REVIEW COMMENT END" `isPrefixOf` (head rest) then 1 else 0
              after = drop skipEnd rest
          in go after acc inBase inFeature  -- Skip entire block
      | inFeature || (not inBase && not inFeature) = go lns (ln : acc) inBase inFeature
      | otherwise = go lns acc inBase inFeature

extractComments :: [String] -> [String] -> [(Int, String)]
extractComments original edited =
  let diffs = getGroupedDiff original edited
      (cmts, al, current) = foldl' (\(cs, al, cur) d ->
        case d of
          Both ls _ ->
            let newCs = if null cur then cs else cs ++ [(if al == 0 then 1 else al, normalizeComment (intercalate "\n" cur))]
            in (newCs, al + length ls, [])
          First ls ->
            (cs, al + length ls, cur)
          Second ls -> (cs, al, cur ++ ls)
        ) ([], 1, []) diffs
      finalCmts = if null current then cmts else cmts ++ [(if al == 0 then 1 else al, normalizeComment (intercalate "\n" current))]
  in filter (\(_, t) -> not (all isSpace t)) finalCmts  -- filter non-empty

data App = App { appGlobal :: Global, appCommand :: Command }

appParser :: Parser App
appParser = App <$> globalParser <*> commandParser

main :: IO ()
main = do
  App global cmd <- execParser $ info (appParser <**> helper) idm
  baseB <- case gBaseBranch global of
    Just b -> return b
    Nothing -> getBaseBranch
  branch <- fmap trimTrailing (readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] "")
  mergeBase <- trimTrailing <$> readProcess "git" ["merge-base", baseB, branch] ""
  reviewer <- fmap trimTrailing (readProcess "git" ["config", "user.name"] "")
  reviewFile <- getReviewFile branch reviewer
  case cmd of
    Start -> do
      mState <- loadReviewState reviewFile
      case mState of
        Just existing -> do
          filesOut <- readProcess "git" ["diff", "--name-only", mergeBase, "--"] ""
          let files = lines filesOut
          let resumed = existing { rsStatus = "active", rsFiles = files, rsCurrentIndex = 0 }
          saveReviewState reviewFile resumed
          putStrLn "Resuming existing review"
          recordPR branch >>= putStrLn
        Nothing -> do
          filesOut <- readProcess "git" ["diff", "--name-only", mergeBase, "--"] ""
          let files = lines filesOut
          let newState = ReviewState "active" 0 files [] branch reviewer
          saveReviewState reviewFile newState
          putStrLn "New review started"
          recordPR branch >>= putStrLn
    Next -> handleNav NavNext reviewFile branch mergeBase Nothing
    Previous -> handleNav NavPrevious reviewFile branch mergeBase Nothing
    Open mbFile -> handleNav NavOpen reviewFile branch mergeBase mbFile
    Files -> do
      mState <- loadReviewState reviewFile
      case mState of
        Just state | rsStatus state == "active" -> do
          let files = rsFiles state
          let current = rsCurrentIndex state
          mapM_ (\(i, f) -> putStrLn $ (if i == current then "> " else "  ") ++ f) (zip [0..] files)
        _ -> do
          out <- readProcess "git" ["diff", "--name-only", mergeBase, "--"] ""
          putStr out
    Changes -> do
      out <- readProcess "git" ["diff", mergeBase, "--"] ""
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
            currentRev <- trimTrailing <$> readProcess "git" ["rev-parse", "HEAD"] ""
            let newComment = Cmt cmtId file line text False "not-solved" Nothing currentRev
            let newState = state { rsComments = rsComments state ++ [newComment] }
            saveReviewState reviewFile newState
            putStrLn $ "Added comment " ++ cmtId
    Resolve rid mbRStatus mbRAnswer -> do
      mState <- loadReviewState reviewFile
      case mState of
        Nothing -> do
          hPutStrLn stderr "No review"
          exitFailure
        Just state -> do
          let updatedComments = map (\c -> if cmId c == rid then c { cmResolved = True, cmStatus = fromMaybe (cmStatus c) mbRStatus, cmAnswer = fromMaybe (cmAnswer c) (Just mbRAnswer) } else c) (rsComments state)
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
    Send -> do
      mState <- loadReviewState reviewFile
      case mState of
        Nothing -> do
          hPutStrLn stderr "No review"
          exitFailure
        Just state -> do
          let comments = rsComments state
          let fullContent = concatMap formatComment comments
          let total = length comments
          let solved = length (filter cmResolved comments)
          let unsolved = total - solved
          let summary = if total == 0 || solved == total
                        then "Review for " ++ branch ++ " by " ++ reviewer ++ ": Everything is solved! 🎉"
                        else "Review for " ++ branch ++ " by " ++ reviewer ++ " attached. Total comments: " ++ show total ++ ", Solved: " ++ show solved ++ ", To solve: " ++ show unsolved ++ "."
          let filename = "review-summary-" ++ sanitizeBranch branch ++ "-" ++ reviewer ++ ".md"
          mbToken <- getSlackToken
          mbChannel <- getSlackChannel
          mbWebhook <- getSlackWebhook
          case (mbToken, mbChannel) of
            (Just token, Just channel) -> sendViaApi summary fullContent filename channel token
            _ -> case mbWebhook of
              Nothing -> do
                hPutStrLn stderr "Slack not configured"
                exitFailure
              Just webhook -> do
                let message = summary ++ "\n" ++ fullContent
                sendViaWebhook webhook message
    Comments withCtx showAll showResolved -> do
      mState <- loadReviewState reviewFile
      case mState of
        Nothing -> do
          hPutStrLn stderr "No review"
          exitFailure
        Just state -> do
          let filteredCmts = if showAll then rsComments state
                             else if showResolved then filter cmResolved (rsComments state)
                             else filter (not . cmResolved) (rsComments state)
          displayComments branch filteredCmts withCtx
    ImportAnswers -> do
      withSystemTempFile "paste.tmp" $ \tmpPath handle -> do
        hPutStr handle ""
        hClose handle
        editor <- fromMaybe "vim" <$> lookupEnv "EDITOR"
        callProcess editor [tmpPath]
        pasted <- readFile tmpPath
        parsedCmts <- PRTools.CommentFormatter.parsePastedMessage pasted
        mState <- loadReviewState reviewFile
        case mState of
          Nothing -> do
            hPutStrLn stderr "No review"
            exitFailure
          Just state -> do
            let updatedComments = foldl' (\cs pc -> map (\c -> if cmId c == cmId pc then c { cmStatus = cmStatus pc, cmAnswer = cmAnswer pc } else c) cs) (rsComments state) parsedCmts
            mapM_ (\pc -> unless (any (\c -> cmId c == cmId pc) (rsComments state)) $ putStrLn $ "Warning: No matching comment for ID " ++ cmId pc) parsedCmts
            let newState = state { rsComments = updatedComments }
            saveReviewState reviewFile newState
            putStrLn "Imported answers for matching comments"

handleNav :: NavAction -> FilePath -> String -> String -> Maybe String -> IO ()
handleNav action rf branch mergeBase mbFile = do
  mState <- loadReviewState rf
  case mState of
    Nothing -> do
      hPutStrLn stderr "No active review"
      exitFailure
    Just state -> if rsStatus state /= "active" then do
      hPutStrLn stderr "No active review"
      exitFailure
      else do
        updatedState <- case mbFile of
          Just filePath -> do
            let files = rsFiles state
            case findIndex (== filePath) files of
              Just idx -> return state { rsCurrentIndex = idx }
              Nothing -> do
                let newFiles = files ++ [filePath]
                return state { rsFiles = newFiles, rsCurrentIndex = length files }
          Nothing -> return state
        let doOpen st = do
              let filePath = rsFiles st !! rsCurrentIndex st
              let fileCmts = filter (\c -> cmFile c == filePath) (rsComments st)
              newCmts <- openEditor filePath branch mergeBase fileCmts
              mLatest <- loadReviewState rf
              let latest = case mLatest of
                    Just l -> l
                    Nothing -> st
              let finalState = latest { rsComments = rsComments latest ++ newCmts }
              saveReviewState rf finalState
              return finalState
        let tryOpen st = catch (doOpen st) (\e -> do
              hPutStrLn stderr $ "Error opening file: " ++ show (e :: IOException)
              return st)
        let (finalState, maybeMsg) = case action of
              NavOpen -> (updatedState, Nothing)
              NavPrevious -> if rsCurrentIndex updatedState > 0
                             then (updatedState { rsCurrentIndex = rsCurrentIndex updatedState - 1 }, Nothing)
                             else (updatedState, Just "No previous files")
              NavNext -> if rsCurrentIndex updatedState < length (rsFiles updatedState) - 1
                         then (updatedState { rsCurrentIndex = rsCurrentIndex updatedState + 1 }, Nothing)
                         else (updatedState, Just "No more files")
        case maybeMsg of
          Just msg -> putStrLn msg
          Nothing -> do
            saveReviewState rf finalState
            _ <- tryOpen finalState
            return ()
