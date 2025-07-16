import Control.Monad (foldM)
import Data.List (drop, foldl', head, length, lines, null, take, takeWhile, words)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Yaml (decodeFileEither)
import Options.Applicative
import System.Environment (getArgs)
import System.FilePath ( (</>) )
import System.FilePath.Glob (glob)
import System.Process (readProcess)
import PRTools.Config (getBaseBranch, reviewDir)
import PRTools.ReviewState (ReviewState(..), Cmt(..))

data Comment = Comment { cReviewer :: String, cText :: String, cResolved :: Bool, cId :: String } deriving Show

type Comments = Map.Map String (Map.Map Int [Comment])

collectComments :: String -> IO Comments
collectComments branch = do
  rfs <- glob (reviewDir </> (branch ++ "-*.yaml"))
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

data Opts = Opts
  { optBranch :: String
  , optBase :: Maybe String
  }

optsParser :: Parser Opts
optsParser = Opts
  <$> strArgument (metavar "BRANCH" <> help "The feature branch")
  <*> optional (strOption (long "base-branch" <> metavar "BASE" <> help "Override the base branch"))

main :: IO ()
main = do
  opts <- execParser $ info (optsParser <**> helper) idm
  baseB <- case optBase opts of
    Just b -> return b
    Nothing -> getBaseBranch
  let branch = optBranch opts
  comms <- collectComments branch
  diffText <- readProcess "git" ["diff", baseB, branch] ""
  let diffLines = lines diffText
  let output = "# Annotated Diff for " ++ branch : "" : "```diff" : go diffLines Nothing 0 comms
  putStr (unlines output)
  where
    go [] _ _ _ = ["```"]
    go (l:ls) cf cl comms = l : (case cf of
      Just f -> fromMaybe [] (do
        cs <- Map.lookup f comms >>= Map.lookup cl
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
