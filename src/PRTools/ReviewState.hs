module PRTools.ReviewState where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Aeson.Key (fromString)
import Data.Aeson.Types ((.!=))
import Data.Yaml (FromJSON(..), decodeFileEither, encodeFile, parseJSON, withObject, (.:), (.:?))
import System.Directory (doesFileExist)

data ReviewState = ReviewState
  { rsStatus :: String
  , rsCurrentIndex :: Int
  , rsFiles :: [String]
  , rsComments :: [Cmt]
  , rsBranch :: String
  , rsReviewer :: String
  } deriving (Eq, Show)

data Cmt = Cmt
  { cmId :: String
  , cmFile :: String
  , cmLine :: Int
  , cmText :: String
  , cmResolved :: Bool
  , cmStatus :: String
  , cmAnswer :: Maybe String
  , cmRevision :: String
  } deriving (Eq, Show)

instance FromJSON ReviewState where
  parseJSON = withObject "ReviewState" $ \v -> ReviewState
    <$> v .: fromString "status"
    <*> v .: fromString "current_index"
    <*> v .: fromString "files"
    <*> v .: fromString "comments"
    <*> v .: fromString "branch"
    <*> v .: fromString "reviewer"

instance ToJSON ReviewState where
  toJSON rs = object
    [ fromString "status" .= rsStatus rs
    , fromString "current_index" .= rsCurrentIndex rs
    , fromString "files" .= rsFiles rs
    , fromString "comments" .= rsComments rs
    , fromString "branch" .= rsBranch rs
    , fromString "reviewer" .= rsReviewer rs
    ]

instance FromJSON Cmt where
  parseJSON = withObject "Cmt" $ \v -> Cmt
    <$> v .: fromString "id"
    <*> v .: fromString "file"
    <*> v .: fromString "line"
    <*> v .: fromString "text"
    <*> v .: fromString "resolved"
    <*> (v .:? fromString "status" .!= "not-solved")
    <*> (v .:? fromString "answer")
    <*> (v .:? fromString "revision" .!= "")

instance ToJSON Cmt where
  toJSON cm = object
    [ fromString "id" .= cmId cm
    , fromString "file" .= cmFile cm
    , fromString "line" .= cmLine cm
    , fromString "text" .= cmText cm
    , fromString "resolved" .= cmResolved cm
    , fromString "status" .= cmStatus cm
    , fromString "answer" .= cmAnswer cm
    , fromString "revision" .= cmRevision cm
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
