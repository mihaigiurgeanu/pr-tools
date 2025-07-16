module PRTools.ReviewState where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Yaml (FromJSON(..), decodeFileEither, encodeFile, parseJSON, withObject, (.:), (.:?))
import System.Directory (doesFileExist)

data ReviewState = ReviewState
  { rsStatus :: String
  , rsCurrentIndex :: Int
  , rsFiles :: [String]
  , rsComments :: [Cmt]
  , rsBranch :: String
  , rsReviewer :: String
  } deriving Show

data Cmt = Cmt
  { cmId :: String
  , cmFile :: String
  , cmLine :: Int
  , cmText :: String
  , cmResolved :: Bool
  } deriving Show

instance FromJSON ReviewState where
  parseJSON = withObject "ReviewState" $ \v -> ReviewState
    <$> v .: "status"
    <*> v .: "current_index"
    <*> v .: "files"
    <*> v .: "comments"
    <*> v .: "branch"
    <*> v .: "reviewer"

instance ToJSON ReviewState where
  toJSON rs = object
    [ "status" .= rsStatus rs
    , "current_index" .= rsCurrentIndex rs
    , "files" .= rsFiles rs
    , "comments" .= rsComments rs
    , "branch" .= rsBranch rs
    , "reviewer" .= rsReviewer rs
    ]

instance FromJSON Cmt where
  parseJSON = withObject "Cmt" $ \v -> Cmt
    <$> v .: "id"
    <*> v .: "file"
    <*> v .: "line"
    <*> v .: "text"
    <*> v .: "resolved"

instance ToJSON Cmt where
  toJSON cm = object
    [ "id" .= cmId cm
    , "file" .= cmFile cm
    , "line" .= cmLine cm
    , "text" .= cmText cm
    , "resolved" .= cmResolved cm
    ]

loadReviewState :: FilePath -> IO (Maybe ReviewState)
loadReviewState rf = do
  exists <- doesFileExist rf
  if not exists
    then return Nothing
    else do
      res <- decodeFileEither rf
      case res of
        Left _ -> return Nothing
        Right val -> return (Just val)

saveReviewState :: FilePath -> ReviewState -> IO ()
saveReviewState = encodeFile
