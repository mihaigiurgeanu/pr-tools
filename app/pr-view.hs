{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception (catch, IOException)
import Control.Monad (foldM)
import Data.List (drop, foldl', head, length, lines, null, take, takeWhile, words, zipWith)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Yaml (decodeFileEither)
import Options.Applicative
import System.Environment (getArgs)
import System.FilePath ( (</>) )
import System.FilePath.Glob (glob)
import System.Process (readProcess)
import PRTools.Config (getBaseBranch, reviewDir, trimTrailing, sanitizeBranch)
import PRTools.ReviewState (ReviewState(..), Cmt(..))

data Comment = Comment { cReviewer :: String, cText :: String, cResolved :: Bool, cId :: String } deriving Show

type Comments = Map.Map String (Map.Map Int [Comment])

collectComments :: String -> IO Comments
collectComments branch = do
  let safeBranch = sanitizeBranch branch
  rfs <- glob (reviewDir </> (safeBranch ++ "-*.yaml"))
  foldM (\acc rf -> do
    mState <- decodeFileEither rf
    case mState of
      Left _ -> return acc
      Right state -> return $ foldl' (\accC c ->
        let file = cmFile c
            line = cmLine c
            newC = Comment (rsReviewer state) (cmText c) (cmResolved c) (cmId c)
        in Map.alter (\maybeInner ->
             let inner = fromMaybe Map.empty maybeInner
                 newInner = Map.alter (\maybeList -> Just (newC : fromMaybe [] maybeList)) line inner
             in Just newInner
           ) file accC
        ) acc (rsComments state)
    ) Map.empty rfs

data Command =
    Diff { dBranch :: Maybe String, dBase :: Maybe String, dFull :: Bool }
  | Comments { cBranch :: Maybe String }

commandParser :: Parser Command
commandParser = subparser
  ( command "diff" (info diffParser (progDesc "View annotated diff (default)"))
 <> command "comments" (info commentsParser (progDesc "List all comments with context"))
  ) <|> (Diff
    <$> optional (strArgument (metavar "BRANCH" <> help "The feature branch (default: current)"))
    <*> optional (strOption (long "base-branch" <> metavar "BASE" <> help "Override the base branch"))
    <*> switch (long "full" <> help "Show full file contents with comments (including unchanged areas)")
  )
  where
    diffParser = Diff
      <$> optional (strArgument (metavar "BRANCH" <> help "The feature branch (default: current)"))
      <*> optional (strOption (long "base-branch" <> metavar "BASE" <> help "Override the base branch"))
      <*> switch (long "full" <> help "Show full file contents with comments (including unchanged areas)")
    commentsParser = Comments
      <$> optional (strArgument (metavar "BRANCH" <> help "The feature branch (default: current)"))

main :: IO ()
main = do
  cmd <- execParser $ info (commandParser <**> helper) idm
  case cmd of
    Diff mbBranch mbBase full -> do
      baseB <- case mbBase of
        Just b -> return b
        Nothing -> getBaseBranch
      branch <- case mbBranch of
        Just b -> return b
        Nothing -> fmap trimTrailing (readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] "")
      comms <- collectComments branch
      if full then do
        let allFiles = Map.keys comms  -- Files with comments
        outputs <- mapM (renderFullFile branch baseB comms) allFiles
        putStr (unlines $ ("# Full Annotated Files for " ++ branch) : concat outputs)
      else do
        diffText <- readProcess "git" ["diff", baseB, branch, "--"] ""
        let diffLines = lines diffText
            go [] _ _ _ = ["```"]
            go (l:ls) cf cl comms = l : (case cf of
              Just f -> fromMaybe [] (do
                inner <- Map.lookup f comms
                cs <- Map.lookup cl inner
                return $ map (\c -> let status = if cResolved c then "RESOLVED" else "COMMENT"
                                        txt = map (\ch -> if ch == '\n' then ' ' else ch) (cText c)
                                    in " # " ++ status ++ " (" ++ cReviewer c ++ ") [id:" ++ cId c ++ "]: " ++ txt) cs
                ) 
              Nothing -> []) ++ if startsWith "diff --git " l then
                let parts = words l
                    newFile = drop 2 (parts !! 3)
                in go ls (Just newFile) 0 comms
              else if startsWith "@@ " l then
                let hunk = words l !! 2
                    start = drop 1 hunk
                    newCl = read (takeWhile (/= ',') start) :: Int
                in go ls cf newCl comms
              else if not (null l) && (l !! 0 == '+' || l !! 0 == ' ') then go ls cf (cl + 1) comms
              else go ls cf cl comms

            startsWith :: String -> String -> Bool
            startsWith pre str = take (length pre) str == pre
            output = ("# Annotated Diff for " ++ branch) : "" : "```diff" : go diffLines Nothing 0 comms
        putStr (unlines output)
    Comments mbBranch -> do
      branch <- case mbBranch of
        Just b -> return b
        Nothing -> fmap trimTrailing (readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] "")
      comms <- collectComments branch
      let allComments = Map.foldrWithKey
            (\file lineMap acc ->
              Map.foldrWithKey
                (\line cs acc' ->
                  acc' ++ map (\c -> (file, line, c)) cs
                )
                acc
                lineMap
            )
            []
            comms
      mapM_ (\(file, line, c) -> do
        content <- readProcess "git" ["show", branch ++ ":" ++ file] "" `catch` (\(_ :: IOException) -> return "")
        let fileLines = lines content
        let start = max 0 (line - 4)
        let context = take 7 (drop start fileLines)
        let numberedContext = zipWith (\i ln -> "  " ++ show (start + 1 + i) ++ ": " ++ ln) [0..] context
        putStrLn $ "File: " ++ file ++ "\nLine: " ++ show line ++ "\nID: " ++ cId c ++ "\nStatus: " ++ (if cResolved c then "resolved" else "unresolved") ++ "\nComment: " ++ cText c ++ "\nContext:\n" ++ unlines numberedContext ++ "\n---"
        ) allComments

renderFullFile :: String -> String -> Comments -> String -> IO [String]
renderFullFile branch baseB comms file = do
  content <- readProcess "git" ["show", branch ++ ":" ++ file] "" `catch` (\(_ :: IOException) -> return "")
  let fileLines = lines content
  baseContent <- readProcess "git" ["show", baseB ++ ":" ++ file] "" `catch` (\(_ :: IOException) -> return "")
  let baseLines = lines baseContent
  let lineComments = fromMaybe Map.empty (Map.lookup file comms)
  let annotated = foldl' (\acc (i, line) ->
				let cs = fromMaybe [] (Map.lookup (i+1) lineComments)  -- 1-based
				    cLines = map (\c -> " # " ++ (if cResolved c then "RESOLVED" else "COMMENT") ++ " (" ++ cReviewer c ++ ") [id:" ++ cId c ++ "]: " ++ cText c) cs
				    prefix = if i < length baseLines && baseLines !! i /= line then "+ " else "  "
				in acc ++ [prefix ++ line] ++ cLines
			      ) [] (zip [0..] fileLines)
  return $ "" : ("## " ++ file) : "```" : annotated ++ ["```"]
