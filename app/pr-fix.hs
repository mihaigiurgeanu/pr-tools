{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (encode, object, (.=))
import Data.List (findIndex, foldl', intercalate, isPrefixOf, nub, sortBy, zipWith)
import Data.List.Split (splitOn)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isSpace, toLower)
import Data.Maybe (fromMaybe, catMaybes, listToMaybe)
import Data.Ord (comparing)
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
import System.IO (hClose, hPutStr, hPutStrLn, stderr)
import System.IO.Temp (withSystemTempFile)
import System.Process (callProcess, readProcess)
import PRTools.Config (getSlackWebhook, trimTrailing, sanitizeBranch, getSlackToken, getSlackChannel)
import PRTools.ReviewState
import PRTools.CommentRenderer
import PRTools.CommentFormatter
import PRTools.PRState (recordPR, recordFixEvent)
import PRTools.Slack (sendViaApi, sendViaWebhook)

import Control.Monad (unless)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

fixDir :: FilePath
fixDir = ".pr-fixes"

getFixFile :: String -> String -> IO FilePath
getFixFile branch fixer = do
  createDirectoryIfMissing False fixDir
  let safeBranch = sanitizeBranch branch
  return $ fixDir </> safeBranch ++ "-fix-" ++ fixer ++ ".yaml"

data FixCommand =
    FStart
  | FComments Bool Bool Bool
  | FFiles
  | FOpen (Maybe String)
  | FNext
  | FPrevious
  | FEnd
  | FSend
  | FResolve { fId :: String, fStatus :: String, fAnswer :: Maybe String }

data Global = Global {}

globalParser :: Parser Global
globalParser = pure Global

commandParser :: Parser FixCommand
commandParser = subparser
  ( command "start" (info (pure FStart <**> helper) (progDesc "Start fix session"))
 <> command "comments" (info (commentsParser <**> helper) (progDesc "Display comments (compact by default)"))
 <> command "files" (info (pure FFiles <**> helper) (progDesc "List files"))
 <> command "open" (info (FOpen <$> optional (strOption (long "file" <> metavar "FILE" <> help "Specific file to open")) <**> helper) (progDesc "Open current or specific file"))
 <> command "next" (info (pure FNext <**> helper) (progDesc "Next file"))
 <> command "previous" (info (pure FPrevious <**> helper) (progDesc "Previous file"))
 <> command "end" (info (pure FEnd <**> helper) (progDesc "End fix session"))
 <> command "send" (info (pure FSend <**> helper) (progDesc "Send fix summary to Slack"))
 <> command "resolve" (info (resolveParser <**> helper) (progDesc "Resolve a comment"))
  )
  where
    commentsParser = FComments
      <$> switch (long "with-context" <> help "Display comments with context")
      <*> switch (long "all" <> help "Display all comments (default: unresolved only)")
      <*> switch (long "resolved" <> help "Display only resolved comments")
    resolveParser = FResolve
      <$> strOption (long "id" <> metavar "ID" <> help "Comment ID")
      <*> strOption (long "status" <> metavar "STATUS" <> help "Status (e.g., solved, not-solved, will-not-solve)")
      <*> optional (strOption (long "answer" <> metavar "ANSWER" <> help "Optional answer/explanation"))

data App = App { appGlobal :: Global, appCommand :: FixCommand }

appParser :: Parser App
appParser = App <$> globalParser <*> commandParser


handleOpen :: FilePath -> String -> IO ()
handleOpen fixFile branch = do
  mState <- loadReviewState fixFile
  case mState of
    Nothing -> do
      hPutStrLn stderr "No active fix session"
      exitFailure
    Just state -> if rsStatus state /= "fixing" then do
      hPutStrLn stderr "No active fix session"
      exitFailure
      else do
        let idx = rsCurrentIndex state
        let files = rsFiles state
        if idx < 0 || idx >= length files then do
          hPutStrLn stderr "Invalid file index"
          exitFailure
          else do
            let file = files !! idx
            status <- readProcess "git" ["status", "--porcelain", file] ""
            if not (null (trim status)) then do
              hPutStrLn stderr $ "Please commit your changes to " ++ file ++ " before using pr-fix open/next/previous to avoid overwriting uncommitted work in that file."
              exitFailure
            else do
              let fileCmts = filter (\c -> cmFile c == file) (rsComments state)
              augmentedContent <- renderForFix branch file fileCmts
              withSystemTempFile "fix.tmp" $ \tmpPath handle -> do
                hPutStr handle augmentedContent
                hClose handle
                editor <- fromMaybe "vim" <$> lookupEnv "EDITOR"
                callProcess editor [tmpPath]
                editedContent <- readFile tmpPath
                if editedContent == augmentedContent then do
                  putStrLn "No changes detected; skipping overwrite."
                else do
                  let editedLines = lines editedContent
                  mLatest <- loadReviewState fixFile
                  let latest = case mLatest of
                        Just l -> l
                        Nothing -> state
                  let (cleanLines, updatedCmts) = parseEditedFix editedLines (rsComments latest)
                  writeFile file (unlines cleanLines)
                  currentRev <- trimTrailing <$> readProcess "git" ["rev-parse", "HEAD"] ""
                  let updatedWithRev = map (\c -> c { cmRevision = currentRev }) updatedCmts
                  let newState = latest { rsComments = updatedWithRev }
                  saveReviewState fixFile newState

parseEditedFix :: [String] -> [Cmt] -> ([String], [Cmt])
parseEditedFix lines cmts = go lines [] cmts
  where
    go [] accLines accCmts = (reverse accLines, accCmts)
    go (ln:lns) accLines accCmts
      | "-- REVIEW COMMENT BEGIN [" `isPrefixOf` ln =
          let (block, rest) = span (\l -> not ("-- REVIEW COMMENT END" `isPrefixOf` l)) lns
              fullBlock = ln : block  -- Exclude the END line if present
              updated = parseBlock fullBlock accCmts
              skipEnd = if not (null rest) && "-- REVIEW COMMENT END" `isPrefixOf` (head rest) then 1 else 0
              afterRest = drop skipEnd rest
          in go afterRest accLines updated  -- Skip block, continue with rest
      | otherwise = go lns (ln : accLines) accCmts

    parseBlock :: [String] -> [Cmt] -> [Cmt]  -- Returns only updated cmts
    parseBlock (header:body) accCmts =
      let cidStart = length ("-- REVIEW COMMENT BEGIN [" :: String)
          cidEnd = findIndex (== ']') (drop cidStart header)  -- Safer than maybe
          cid = case cidEnd of
                  Just end -> take 8 (drop cidStart header)
                  Nothing -> ""
          afterCid = maybe "" (\end -> drop (cidStart + end + 1) header) cidEnd  -- +1 for ]
          statusStart = findSub "[status:" afterCid
          afterStatusLabel = maybe afterCid (\start -> drop (start + 8) afterCid) statusStart
          statusEnd = findSub "]" afterStatusLabel
          status = maybe "not-solved" (\end -> take end afterStatusLabel) statusEnd
          afterStatus = maybe afterStatusLabel (\end -> drop (end + 1) afterStatusLabel) statusEnd
          answerStart = findSub "[answer:" afterStatus
          headerAnswer = maybe Nothing (\start -> Just (drop (start + 8) afterStatus)) answerStart  -- Up to end of header
          -- Body: Split comment text and any multi-line answer
          (textLines, answerLines) = span (not . ("[answer:" `isPrefixOf`)) body
          text = intercalate "\n" (map trim textLines)
          bodyAnswer = if null answerLines then Nothing else Just (intercalate "\n" (map trim (tail answerLines)))  -- Skip [answer: line if present
          finalAnswer = case (headerAnswer, bodyAnswer) of
                          (Just ha, Just ba) -> Just (ha ++ "\n" ++ ba)  -- Combine if both
                          (Just ha, Nothing) -> Just ha
                          (Nothing, Just ba) -> Just ba
                          _ -> Nothing
          updated = map (\c -> if cmId c == cid then c { cmText = if null text then cmText c else text, cmStatus = status, cmAnswer = finalAnswer } else c) accCmts
      in updated
    parseBlock _ accCmts = accCmts  -- Invalid block, skip

    findSub :: String -> String -> Maybe Int
    findSub sub str = go 0 str
      where go _ [] = Nothing
            go i xs = if sub `isPrefixOf` xs then Just i else go (i+1) (tail xs)

data NavAction = NavNext | NavPrevious | NavOpen

handleNav :: NavAction -> FilePath -> String -> Maybe String -> IO ()
handleNav action ff branch mbFile = do
  mState <- loadReviewState ff
  case mState of
    Nothing -> do
      hPutStrLn stderr "No active fix session"
      exitFailure
    Just state -> if rsStatus state /= "fixing" then do
      hPutStrLn stderr "No active fix session"
      exitFailure
      else do
        updatedState <- case mbFile of
          Just filePath -> do
            let files = rsFiles state
            case findIndex (== filePath) files of
              Just idx -> return state { rsCurrentIndex = idx }
              Nothing -> do
                hPutStrLn stderr $ "File " ++ filePath ++ " not in fix session"
                exitFailure
          Nothing -> return state
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
            saveReviewState ff finalState
            handleOpen ff branch

main :: IO ()
main = do
  App _ cmd <- execParser $ info (appParser <**> helper) idm
  branch <- fmap trimTrailing (readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] "")
  fixer <- fmap trimTrailing (readProcess "git" ["config", "user.name"] "")
  fixFile <- getFixFile branch fixer
  case cmd of
    FStart -> do
      exists <- doesFileExist fixFile
      if exists then do
        mState <- loadReviewState fixFile
        case mState of
          Just state | rsStatus state == "fixing" -> do
            putStrLn "Active fix session found. Overwrite? (y/n)"
            response <- getLine
            unless (trim (map toLower response) == "y") $ do
              putStrLn "Aborted."
              exitFailure
          _ -> return ()
      else return ()
      withSystemTempFile "paste.tmp" $ \tmpPath handle -> do
        hPutStr handle ""
        hClose handle
        editor <- fromMaybe "vim" <$> lookupEnv "EDITOR"
        callProcess editor [tmpPath]
        pasted <- readFile tmpPath
        parsedCmts <- parsePastedMessage pasted
        currentRev <- trimTrailing <$> readProcess "git" ["rev-parse", "HEAD"] ""
        let updatedCmts = map (\c -> if null (cmRevision c) then c { cmRevision = currentRev } else c) parsedCmts
        let uniqueFiles = nub (map cmFile updatedCmts)
        let state = ReviewState "fixing" 0 uniqueFiles updatedCmts branch fixer
        saveReviewState fixFile state
        recordFixEvent branch fixer "start"
        putStrLn "Fix session started"
        recordPR branch >>= putStrLn
    FComments withCtx showAll showResolved -> do
      mState <- loadReviewState fixFile
      case mState of
        Just state | rsStatus state == "fixing" -> do
          let filteredCmts = if showAll then rsComments state
                             else if showResolved then filter cmResolved (rsComments state)
                             else filter (not . cmResolved) (rsComments state)
          displayComments (rsBranch state) filteredCmts withCtx
        _ -> hPutStrLn stderr "No active fix session"
    FFiles -> do
      mState <- loadReviewState fixFile
      case mState of
        Just state | rsStatus state == "fixing" -> do
          let files = rsFiles state
          let current = rsCurrentIndex state
          mapM_ (\(i, f) -> putStrLn $ (if i == current then "> " else "  ") ++ f) (zip [0..] files)
        _ -> hPutStrLn stderr "No active fix session"
    FOpen mbFile -> handleNav NavOpen fixFile branch mbFile
    FNext -> handleNav NavNext fixFile branch Nothing
    FPrevious -> handleNav NavPrevious fixFile branch Nothing
    FResolve rid status mbAnswer -> do
      mState <- loadReviewState fixFile
      case mState of
        Nothing -> hPutStrLn stderr "No fix session"
        Just state -> do
          let updatedComments = map (\c -> if cmId c == rid then c { cmStatus = status, cmAnswer = mbAnswer } else c) (rsComments state)
          if updatedComments == rsComments state
            then putStrLn "Comment not found"
            else do
              let newState = state { rsComments = updatedComments }
              saveReviewState fixFile newState
              putStrLn $ "Updated " ++ rid ++ " to " ++ status
    FEnd -> do
      mState <- loadReviewState fixFile
      case mState of
        Nothing -> hPutStrLn stderr "No fix session"
        Just state -> do
          status <- readProcess "git" ["status", "--porcelain"] ""
          if null (trimTrailing status)
            then do
              let newState = state { rsStatus = "fixed" }
              saveReviewState fixFile newState
              recordFixEvent branch fixer "end"
              putStrLn "Fix session ended"
            else do
              hPutStrLn stderr "Please commit your changes before ending the fix session."
              exitFailure
    FSend -> do
      mState <- loadReviewState fixFile
      case mState of
        Nothing -> hPutStrLn stderr "No fix session"
        Just state -> do
          let comments = rsComments state
          let fullContent = concatMap formatComment comments
          let total = length comments
          let solved = length (filter (\c -> cmStatus c == "solved") comments)
          let answered = length (filter (isJust . cmAnswer) comments)
          let unsolved = length (filter (\c -> cmStatus c == "not-solved") comments)
          let statusCounts = foldl' (\m c -> Map.insertWith (+) (cmStatus c) 1 m) Map.empty comments
          let breakdown = intercalate ", " [k ++ ": " ++ show v | (k, v) <- Map.toList statusCounts]
          let happyAllAnswered = if answered == total then "All comments were answered! 🎉" else ""
          let happyAllSolved = if solved == total then "Everything has been solved! 🎉" else ""
          let happyNoUnsolved = if unsolved == 0 then "No unsolved comments! 🎉" else ""
          let stats = "Solved: " ++ show solved ++ ", Answered: " ++ show answered ++ ", Status breakdown: " ++ breakdown
          let summaryParts = filter (not . null) [happyAllAnswered, happyAllSolved, happyNoUnsolved, stats]
          let summary = "Fix summary for " ++ branch ++ " by " ++ fixer ++ " attached. " ++ intercalate " " summaryParts
          let filename = "fix-summary-" ++ sanitizeBranch branch ++ "-" ++ fixer ++ ".md"
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
