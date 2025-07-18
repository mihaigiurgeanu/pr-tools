{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (encode, object, (.=))
import Data.List (foldl', intercalate, isPrefixOf, nub, sortBy, zipWith)
import Data.List.Split (splitOn)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isSpace)
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
import PRTools.Config (getSlackWebhook, trimTrailing, sanitizeBranch)
import PRTools.ReviewState
import PRTools.CommentRenderer

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
  | FComments Bool
  | FFiles
  | FOpen
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
  ( command "start" (info (pure FStart) (progDesc "Start fix session"))
 <> command "comments" (info commentsParser (progDesc "Display comments (compact by default)"))
 <> command "files" (info (pure FFiles) (progDesc "List files"))
 <> command "open" (info (pure FOpen) (progDesc "Open current file"))
 <> command "next" (info (pure FNext) (progDesc "Next file"))
 <> command "previous" (info (pure FPrevious) (progDesc "Previous file"))
 <> command "end" (info (pure FEnd) (progDesc "End fix session"))
 <> command "send" (info (pure FSend) (progDesc "Send fix summary to Slack"))
 <> command "resolve" (info resolveParser (progDesc "Resolve a comment"))
  )
  where
    commentsParser = FComments
      <$> switch (long "with-context" <> help "Display comments with context")
    resolveParser = FResolve
      <$> strOption (long "id" <> metavar "ID" <> help "Comment ID")
      <*> strOption (long "status" <> metavar "STATUS" <> help "Status (e.g., solved, not-solved, will-not-solve)")
      <*> optional (strOption (long "answer" <> metavar "ANSWER" <> help "Optional answer/explanation"))

data App = App { appGlobal :: Global, appCommand :: FixCommand }

appParser :: Parser App
appParser = App <$> globalParser <*> commandParser

parsePastedMessage :: String -> IO [Cmt]
parsePastedMessage content = do
  let sections = filter (not . all isSpace . unlines . lines) $ splitOn "---" content
  putStrLn $ "Found " ++ show (length sections) ++ " sections to parse."
  parsedSections <- mapM parseSection sections
  let validCmts = catMaybes parsedSections
  if null validCmts
    then putStrLn "No valid comments parsed from any sections."
    else putStrLn $ "Successfully parsed " ++ show (length validCmts) ++ " comments."
  return validCmts

parseSection :: String -> IO (Maybe Cmt)
parseSection s = do
  let allLs = lines s
  let ls = dropWhile (\ln -> take 5 (trim ln) /= "File:") allLs
  if null ls then do
    putStrLn "Section skipped: no 'File:' found."
    return Nothing
    else do
    let f = head ls
    if take 5 (trim f) /= "File:" then do
      putStrLn "Section skipped: invalid 'File:' line."
      return Nothing
      else do
      let file = parseLine "File:" f
      if null file then do
        putStrLn "Section skipped: empty file name."
        return Nothing
        else do
        let rest = tail ls
        let idLine = findLine "ID:" rest
        let statusLine = findLine "Status:" rest
        let resolvedLine = findLine "Resolved:" rest
        let revisionLine = findLine "Revision:" rest
        let lineLine = findLine "Line:" rest
        let commentLine = findLine "Comment:" rest
        case (lineLine, commentLine) of
          (Just l_, Just text) -> do
            let lineStr = parseLine "Line:" l_
            case reads lineStr of
              [(line, "")] -> do
                let cid = fromMaybe "" (parseLine "ID:" <$> idLine)
                let status = fromMaybe "not-solved" (parseLine "Status:" <$> statusLine)
                let resolved = maybe False (== "True") (parseLine "Resolved:" <$> resolvedLine)
                let revision = fromMaybe "" (parseLine "Revision:" <$> revisionLine)
                u <- if null cid then nextRandom else return undefined  -- Use existing ID if present
                let finalId = if null cid then take 8 $ filter (/= '-') $ toString u else cid
                return $ Just $ Cmt finalId file line text resolved status Nothing revision
              _ -> return Nothing
          _ -> return Nothing
  where
    parseLine :: String -> String -> String
    parseLine prefix l = trim (drop (length prefix) l)
    findLine prefix ls' = listToMaybe [ln | ln <- ls', prefix `isPrefixOf` trim ln]

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
            let fileCmts = filter (\c -> cmFile c == file) (rsComments state)
            augmentedContent <- renderForFix branch file fileCmts
            withSystemTempFile "fix.tmp" $ \tmpPath handle -> do
              hPutStr handle augmentedContent
              hClose handle
              editor <- fromMaybe "vim" <$> lookupEnv "EDITOR"
              callProcess editor [tmpPath]
              editedContent <- readFile tmpPath
              let editedLines = lines editedContent
              let (cleanLines, updatedCmts) = foldl' (\(cls, ucs) el ->
                    if take 19 el == "-- REVIEW COMMENT [" then
                      let rest = drop 19 el
                          cid = take 8 rest
                          afterCid = drop 9 rest  -- drop 8 id + ]
                          textEndM = findSub " [status:" afterCid
                          textEnd = fromMaybe (length afterCid) textEndM
                          text_ = take textEnd afterCid  -- includes ": " prefix? Wait, afterCid starts with ": "
                          text = drop 2 text_  -- drop ": "
                          afterText = drop textEnd afterCid
                          statusStartLen = length (" [status:" :: String)
                          statusEndM = findSub "]" (drop statusStartLen afterText)
                          statusEnd = fromMaybe (length afterText - statusStartLen) statusEndM
                          status = take statusEnd (drop statusStartLen afterText)
                          afterStatus = drop (statusStartLen + statusEnd + 1) afterText  -- +1 for ]
                          answer = if null afterStatus then Nothing else let
                                     answerStartLen = length (" [answer:" :: String)
                                     answerEndM = findSub "]" (drop answerStartLen afterStatus)
                                     answerEnd = fromMaybe (length afterStatus - answerStartLen) answerEndM
                                   in Just (take answerEnd (drop answerStartLen afterStatus))
                          updatedC = map (\c -> if cmId c == cid then c { cmStatus = status, cmAnswer = answer, cmResolved = (status == "solved") } else c) ucs
                      in (cls, updatedC)
                    else (cls ++ [el], ucs)
                    ) ([], rsComments state) editedLines
              writeFile file (unlines cleanLines)
              currentRev <- trimTrailing <$> readProcess "git" ["rev-parse", "HEAD"] ""
              let updatedWithRev = map (\c -> c { cmRevision = currentRev }) updatedCmts
              let newState = state { rsComments = updatedWithRev }
              saveReviewState fixFile newState
  where
    findSub :: String -> String -> Maybe Int
    findSub sub str = go 0 str
      where
        go _ [] = Nothing
        go i xs = if sub `isPrefixOf` xs then Just i else go (i+1) (tail xs)

data NavAction = NavNext | NavPrevious | NavOpen

handleNav :: NavAction -> FilePath -> String -> IO ()
handleNav action ff branch = do
  mState <- loadReviewState ff
  case mState of
    Nothing -> do
      hPutStrLn stderr "No active fix session"
      exitFailure
    Just state -> if rsStatus state /= "fixing" then do
      hPutStrLn stderr "No active fix session"
      exitFailure
      else do
        let (updatedState, maybeMsg) = case action of
              NavOpen -> (state, Nothing)
              NavPrevious -> if rsCurrentIndex state > 0
                             then (state { rsCurrentIndex = rsCurrentIndex state - 1 }, Nothing)
                             else (state, Just "No previous files")
              NavNext -> if rsCurrentIndex state < length (rsFiles state) - 1
                         then (state { rsCurrentIndex = rsCurrentIndex state + 1 }, Nothing)
                         else (state, Just "No more files")
        case maybeMsg of
          Just msg -> putStrLn msg
          Nothing -> do
            saveReviewState ff updatedState
            handleOpen ff branch

main :: IO ()
main = do
  App _ cmd <- execParser $ info (appParser <**> helper) idm
  branch <- fmap trimTrailing (readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] "")
  fixer <- fmap trimTrailing (readProcess "git" ["config", "user.name"] "")
  fixFile <- getFixFile branch fixer
  case cmd of
    FStart -> do
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
        putStrLn "Fix session started"
    FComments withCtx -> do
      mState <- loadReviewState fixFile
      case mState of
        Just state | rsStatus state == "fixing" -> do
          let comments = rsComments state
          let branch = rsBranch state
          if withCtx then
            mapM_ (\c -> do
              content <- readProcess "git" ["show", branch ++ ":" ++ cmFile c] ""
              let fileLines = lines content
              let start = max 0 (cmLine c - 4)
              let context = take 7 (drop start fileLines)
              let numberedContext = zipWith (\i ln -> "  " ++ show (start + 1 + i) ++ ": " ++ ln) [0..] context
              putStrLn $ "File: " ++ cmFile c ++ "\nLine: " ++ show (cmLine c) ++ "\nID: " ++ cmId c ++ "\nStatus: " ++ cmStatus c ++ "\nComment: " ++ cmText c ++ "\nAnswer: " ++ fromMaybe "" (cmAnswer c) ++ "\nContext:\n" ++ unlines numberedContext ++ "\n---"
              ) comments
            else
            mapM_ (\c -> putStrLn $ cmFile c ++ ":" ++ show (cmLine c) ++ " [" ++ cmId c ++ "] - " ++ map (\ch -> if ch == '\n' then ' ' else ch) (cmText c) ++ " [" ++ cmStatus c ++ "]" ++ maybe "" (\a -> " answer: " ++ map (\ch -> if ch == '\n' then ' ' else ch) a) (cmAnswer c)) comments
        _ -> hPutStrLn stderr "No active fix session"
    FFiles -> do
      mState <- loadReviewState fixFile
      case mState of
        Just state | rsStatus state == "fixing" -> do
          let files = rsFiles state
          let current = rsCurrentIndex state
          mapM_ (\(i, f) -> putStrLn $ (if i == current then "> " else "  ") ++ f) (zip [0..] files)
        _ -> hPutStrLn stderr "No active fix session"
    FOpen -> handleNav NavOpen fixFile branch
    FNext -> handleNav NavNext fixFile branch
    FPrevious -> handleNav NavPrevious fixFile branch
    FResolve rid status mbAnswer -> do
      mState <- loadReviewState fixFile
      case mState of
        Nothing -> hPutStrLn stderr "No fix session"
        Just state -> do
          let updatedComments = map (\c -> if cmId c == rid then c { cmStatus = status, cmResolved = (status == "solved"), cmAnswer = mbAnswer } else c) (rsComments state)
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
          let commentTexts = map (\c ->
                                        "File: " ++ cmFile c ++ "\nLine: " ++ show (cmLine c) ++ "\nID: " ++ cmId c ++ "\nStatus: " ++ cmStatus c ++ "\nResolved: " ++ show (cmResolved c) ++ "\nRevision: " ++ cmRevision c ++ "\nComment: " ++ cmText c ++ "\nAnswer: " ++ fromMaybe "" (cmAnswer c) ++ "\n---\n"
                                    ) comments
          let message = "Fix summary for " ++ branch ++ " by " ++ fixer ++ ":\n" ++ concat commentTexts
          mbWebhook <- getSlackWebhook
          case mbWebhook of
            Nothing -> do
              hPutStrLn stderr "Slack webhook not configured"
              exitFailure
            Just webhook -> do
              manager <- newManager tlsManagerSettings
              initReq <- parseRequest webhook
              let req = initReq
                    { method = "POST"
                    , requestBody = RequestBodyLBS $ encode $ object ["text" .= message]
                    , requestHeaders = [("Content-Type", "application/json")]
                    }
              response <- httpLbs req manager
              if statusCode (responseStatus response) == 200
                then putStrLn "Fix summary sent to Slack"
                else hPutStrLn stderr "Error sending to Slack"
