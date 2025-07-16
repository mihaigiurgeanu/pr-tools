import Data.List (words)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Yaml (FromJSON(..), ToJSON, decodeFileEither, parseJSON, withObject, (.:))
import Data.Aeson (object, (.=))
import System.FilePath ( (</>) )
import System.FilePath.Glob (glob)
import System.Process (readProcess)

reviewDir :: FilePath
reviewDir = ".pr-reviews"

baseBranch :: String
baseBranch = "main"

data Comment = Comment { cReviewer :: String, cText :: String, cResolved :: Bool, cId :: String } deriving Show

type Comments = Map.Map String (Map.Map Int [Comment])

collectComments :: String -> IO Comments
collectComments branch = do
  rfs <- glob (reviewDir </> (branch ++ "-*.yaml"))
  foldl (\accM rf -> do
    acc <- accM
    mState <- decodeFileEither rf
    case mState of
      Left _ -> return acc
      Right state -> do
        let reviewer = sReviewer state
        foldl (\accC c -> 
          let file = cFile c
              line = cLine c
              newC = Comment reviewer (cText c) (cResolved c) (cId c)
          in Map.adjust (Map.adjust (newC :) line) file accC
          ) acc (sComments state)
    ) (return Map.empty) rfs

data ReviewState = ReviewState { sReviewer :: String, sComments :: [Cmt] } deriving Show

data Cmt = Cmt { cFile :: String, cLine :: Int, cText :: String, cResolved :: Bool, cId :: String } deriving Show

instance FromJSON ReviewState where
  parseJSON = withObject "ReviewState" $ \v -> ReviewState <$> v .: "reviewer" <*> v .: "comments"

instance FromJSON Cmt where
  parseJSON = withObject "Cmt" $ \v -> Cmt <$> v .: "file" <*> v .: "line" <*> v .: "text" <*> v .: "resolved" <*> v .: "id"

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn "Usage: pr-view <branch>"
    else do
      let branch = head args
      comms <- collectComments branch
      diffText <- readProcess "git" ["diff", baseBranch, branch] ""
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
