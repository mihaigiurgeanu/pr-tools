{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (encode, object, (.=))
import Data.List (foldl', intercalate, isPrefixOf, nub, sortBy)
import Data.List.Split (splitOn)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isSpace)
import Data.Maybe (fromMaybe, catMaybes)
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
  | FComments
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
 <> command "comments" (info (pure FComments) (progDesc "Display comments"))
 <> command "files" (info (pure FFiles) (progDesc "List files"))
 <> command "open" (info (pure FOpen) (progDesc "Open current file"))
 <> command "next" (info (pure FNext) (progDesc "Next file"))
 <> command "previous" (info (pure FPrevious) (progDesc "Previous file"))
 <> command "end" (info (pure FEnd) (progDesc "End fix session"))
 <> command "send" (info (pure FSend) (progDesc "Send fix summary to Slack"))
 <> command "resolve" (info resolveParser (progDesc "Resolve a comment"))
  )
  where
    resolveParser = FResolve
      <$> strOption (long "id" <> metavar "ID" <> help "Comment ID")
      <*> strOption (long "status" <> metavar "STATUS" <> help "Status (e.g., solved, not-solved, will-not-solve)")
      <*> optional (strOption (long "answer" <> metavar "ANSWER" <> help "Optional answer/explanation"))

data App = App { appGlobal :: Global, appCommand :: FixCommand }

appParser :: Parser App
appParser = App <$> globalParser <*> commandParser

parsePastedMessage :: String -> IO [Cmt]
parsePastedMessage content = do
  let sections = splitOn "---" content
  parsedSections <- mapM parseSection sections
  return $ catMaybes parsedSections

parseSection :: String -> IO (Maybe Cmt)
parseSection s = do
  let allLs = lines s
  let ls = dropWhile (\ln -> take 5 (trim ln) /= "File:") allLs
  if null ls then return Nothing else do
    let f = head ls
    if take 5 (trim f) /= "File:" then return Nothing else do
      let file = parseLine "File:" f
      let rest1 = tail ls
      if null rest1 then return Nothing else do
        let l_ = head rest1
        if take 5 (trim l_) /= "Line:" then return Nothing else do
          let lineStr = parseLine "Line:" l_
          case reads lineStr of
            [(line, "")] -> do
              let rest2 = tail rest1
              let commentLines = map trim rest2
              let text = intercalate "\n" (filter (not . null) commentLines)
              if null text then return Nothing else do
                u <- nextRandom
                let cid = take 8 $ filter (/= '-') $ toString u
                return $ Just $ Cmt cid file line text False "not-solved" Nothing
            _ -> return Nothing
  where
    parseLine :: String -> String -> String
    parseLine prefix l = trim (drop (length prefix) (dropWhile (/= ':') l))

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
            content <- readFile file  -- assume on branch
            let fileLines = lines content
            let fileCmts = filter (\c -> cmFile c == file) (rsComments state)
            let sortedCmts = sortBy (comparing cmLine) fileCmts
            let insertComments acc [] = acc
                insertComments acc (c:cs) = 
                  let before = take (cmLine c - 1) acc
                      theLine = if cmLine c - 1 < length acc then [acc !! (cmLine c - 1)] else [""]
                      marker = "-- REVIEW COMMENT [" ++ cmId c ++ "]: " ++ cmText c ++ " [status:" ++ cmStatus c ++ "] [answer:" ++ fromMaybe "" (cmAnswer c) ++ "]"
                      after = drop (cmLine c - 1) acc
                  in insertComments (before ++ theLine ++ [marker] ++ after) cs
            let augmentedLines = insertComments fileLines sortedCmts
            let augmentedContent = unlines augmentedLines
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
              let newState = state { rsComments = updatedCmts }
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
        let uniqueFiles = nub (map cmFile parsedCmts)
        let state = ReviewState "fixing" 0 uniqueFiles parsedCmts branch fixer
        saveReviewState fixFile state
        putStrLn "Fix session started"
    FComments -> do
      mState <- loadReviewState fixFile
      case mState of
        Just state | rsStatus state == "fixing" -> mapM_ (\c -> putStrLn $ cmFile c ++ ":" ++ show (cmLine c) ++ " - " ++ cmText c ++ " [" ++ cmStatus c ++ "] " ++ fromMaybe "" (cmAnswer c)) (rsComments state)
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
                "File: " ++ cmFile c ++ "\nLine: " ++ show (cmLine c) ++ "\nComment: " ++ cmText c ++ "\nStatus: " ++ cmStatus c ++ "\nAnswer: " ++ fromMaybe "" (cmAnswer c) ++ "\n---\n"
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
