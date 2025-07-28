{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (catch, IOException)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.Algorithm.Diff (PolyDiff(..), getGroupedDiff)
import Data.Char (isSpace)
import Data.List (filter, foldl', intercalate, zipWith)
import Data.Maybe (fromMaybe)
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
import PRTools.Config (getBaseBranch, getSlackWebhook, reviewDir, trimTrailing, sanitizeBranch)
import PRTools.ReviewState
import PRTools.CommentRenderer
import PRTools.PRState (recordPR)

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
  | Resolve { rId :: String, rAnswer :: Maybe String }
  | End
  | List
  | Send
  | Comments Bool

data NavAction = NavNext | NavPrevious | NavOpen

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
 <> command "send" (info (pure Send) (progDesc "Send review to Slack"))
 <> command "comments" (info commentsParser (progDesc "List all comments (compact by default)"))
  )
  where
    commentParser = Comment
      <$> strOption (long "file" <> metavar "FILE")
      <*> option auto (long "line" <> metavar "LINE")
      <*> strOption (long "text" <> metavar "TEXT")
    resolveParser = Resolve
      <$> strOption (long "id" <> metavar "ID")
      <*> optional (strOption (long "answer" <> metavar "ANSWER" <> help "Optional answer/explanation"))
    commentsParser = Comments
      <$> switch (long "with-context" <> help "Display comments with context")

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
      return $ Cmt cid filePath l t False "not-solved" Nothing currentRev
      ) newPairs

-- Reused helpers
extractEditedFeature :: [String] -> [String]
extractEditedFeature editedLines =
  let (feat, _, _) = foldl' (\(f, inb, inf) line ->
                                 if line == "<<<<<<< BASE" then (f, True, False)
                                 else if line == "=======" then (f, False, True)
                                 else if line == ">>>>>>> FEATURE" then (f, False, False)
                                 else if take 12 line == "-- COMMENT [" then (f, inb, inf)  -- skip existing comments
                                 else if inf || (not inb && not inf) then (f ++ [line], inb, inf)
                                 else (f, inb, inf)
                            ) ([], False, False) editedLines
  in feat

extractComments :: [String] -> [String] -> [(Int, String)]
extractComments original edited =
  let diffs = getGroupedDiff original edited
      (cmts, al, current) = foldl' (\(cs, al, cur) d ->
        case d of
          Both ls _ ->
            let newCs = if null cur then cs else cs ++ [(if al == 0 then 1 else al, intercalate "\n" cur)]
            in (newCs, al + length ls, [])
          First ls ->
            (cs, al + length ls, cur)
          Second ls -> (cs, al, cur ++ ls)
        ) ([], 1, []) diffs
      finalCmts = if null current then cmts else cmts ++ [(if al == 0 then 1 else al, intercalate "\n" current)]
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
    Next -> handleNav NavNext reviewFile branch mergeBase
    Previous -> handleNav NavPrevious reviewFile branch mergeBase
    Open -> handleNav NavOpen reviewFile branch mergeBase
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
    Resolve rid rAnswer -> do
      mState <- loadReviewState reviewFile
      case mState of
        Nothing -> do
          hPutStrLn stderr "No review"
          exitFailure
        Just state -> do
          let updatedComments = map (\c -> if cmId c == rid then c { cmResolved = True, cmStatus = "solved", cmAnswer = rAnswer } else c) (rsComments state)
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
          let commentTexts = map (\c ->
					"File: " ++ cmFile c ++ "\nLine: " ++ show (cmLine c) ++ "\nID: " ++ cmId c ++ "\nStatus: " ++ cmStatus c ++ "\nResolved: " ++ show (cmResolved c) ++ "\nRevision: " ++ cmRevision c ++ "\nComment: " ++ cmText c ++ "\nAnswer: " ++ fromMaybe "" (cmAnswer c) ++ "\n---\n"
				    ) comments
          let message = "Review for " ++ branch ++ " by " ++ reviewer ++ ":\n" ++ concat commentTexts
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
                then putStrLn "Review sent to Slack"
                else hPutStrLn stderr "Error sending to Slack"
    Comments withCtx -> do
      mState <- loadReviewState reviewFile
      case mState of
        Nothing -> do
          hPutStrLn stderr "No review"
          exitFailure
        Just state -> displayComments branch (rsComments state) withCtx

handleNav :: NavAction -> FilePath -> String -> String -> IO ()
handleNav action rf branch mergeBase = do
  mState <- loadReviewState rf
  case mState of
    Nothing -> do
      hPutStrLn stderr "No active review"
      exitFailure
    Just state -> if rsStatus state /= "active" then do
      hPutStrLn stderr "No active review"
      exitFailure
      else do
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
        case action of
          NavOpen -> do
            _ <- tryOpen state
            return ()
          NavPrevious -> 
            if rsCurrentIndex state > 0 then do
              let newState = state { rsCurrentIndex = rsCurrentIndex state - 1 }
              saveReviewState rf newState
              _ <- tryOpen newState
              return ()
            else putStrLn "No previous files"
          NavNext ->
            if rsCurrentIndex state < length (rsFiles state) - 1 then do
              let newState = state { rsCurrentIndex = rsCurrentIndex state + 1 }
              saveReviewState rf newState
              _ <- tryOpen newState
              return ()
            else putStrLn "No more files"
