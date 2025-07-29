{-# LANGUAGE OverloadedStrings #-}

module PRTools.Slack where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson (Value(..), encode, object, (.=), decode)
import qualified Data.Aeson.KeyMap as KM
import Network.HTTP.Client (RequestBody(RequestBodyLBS), httpLbs, method, newManager, parseRequest, requestBody, requestHeaders, responseBody, responseStatus, urlEncodedBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (statusCode)
import System.IO (hPutStrLn, stderr)
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.CaseInsensitive (mk)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as BSC

sendViaWebhook :: String -> String -> IO ()
sendViaWebhook webhook message = do
  manager <- newManager tlsManagerSettings
  initReq <- parseRequest webhook
  let req = initReq
        { method = "POST"
        , requestBody = RequestBodyLBS $ encode $ object ["text" .= message]
        , requestHeaders = [(mk (TE.encodeUtf8 (T.pack "Content-Type")), TE.encodeUtf8 (T.pack "application/json"))]
        }
  response <- httpLbs req manager
  if statusCode (responseStatus response) == 200
    then putStrLn "Sent to Slack"
    else hPutStrLn stderr "Error sending to Slack"

sendViaApi :: String -> String -> String -> String -> String -> IO ()
sendViaApi summary fileContent filename channel token = do
  manager <- newManager tlsManagerSettings

  -- Step 1: Get upload URL
  initUrlReq <- parseRequest "https://slack.com/api/files.getUploadURLExternal"
  let pairs = [ ("filename", BSC.pack filename)
              , ("length", BSC.pack $ show $ BS.length (TE.encodeUtf8 (T.pack fileContent)))
              ]
  let urlReq = urlEncodedBody pairs initUrlReq
        { method = "POST"
        , requestHeaders =
          [ (mk "Authorization", TE.encodeUtf8 (T.pack ("Bearer " ++ token)))
          ]
        }
  urlResp <- httpLbs urlReq manager
  let urlStatus = statusCode (responseStatus urlResp)
  if urlStatus /= 200
    then hPutStrLn stderr $ "HTTP error getting upload URL: " ++ show urlStatus
    else case decode (responseBody urlResp) of
      Just (Object val) -> case (KM.lookup "ok" val, KM.lookup "upload_url" val, KM.lookup "file_id" val) of
        (Just (Bool True), Just (String url), Just (String fileId)) -> do
          -- Step 2: Upload file to URL
          initUploadReq <- parseRequest (T.unpack url)
          let uploadReq = initUploadReq
                { method = "POST"
                , requestBody = RequestBodyLBS (LBS.fromStrict (TE.encodeUtf8 (T.pack fileContent)))
                , requestHeaders = [(mk "Content-Type", "application/octet-stream")]
                }
          uploadResp <- httpLbs uploadReq manager
          let uploadStatus = statusCode (responseStatus uploadResp)
          if uploadStatus /= 200
            then hPutStrLn stderr $ "HTTP error uploading file: " ++ show uploadStatus
            else do
              -- Step 3: Complete upload
              initCompleteReq <- parseRequest "https://slack.com/api/files.completeUploadExternal"
              let completeReq = initCompleteReq
                    { method = "POST"
                    , requestBody = RequestBodyLBS $ encode $ object
                      [ "files" .= [object ["id" .= fileId, "title" .= filename]]
                      , "channel_id" .= channel
                      , "initial_comment" .= summary
                      ]
                    , requestHeaders =
                      [ (mk "Authorization", TE.encodeUtf8 (T.pack ("Bearer " ++ token)))
                      , (mk "Content-Type", "application/json")
                      ]
                    }
              completeResp <- httpLbs completeReq manager
              let completeStatus = statusCode (responseStatus completeResp)
              if completeStatus /= 200
                then hPutStrLn stderr $ "HTTP error completing upload: " ++ show completeStatus
                else case decode (responseBody completeResp) of
                  Just (Object cVal) -> case KM.lookup "ok" cVal of
                    Just (Bool True) -> putStrLn "Sent to Slack as attachment"
                    _ -> do
                      let errMsg = fromMaybe "Unknown error" (KM.lookup "error" cVal >>= \v -> case v of {String s -> Just (T.unpack s); _ -> Nothing})
                      hPutStrLn stderr $ "Slack API error: " ++ errMsg
                  _ -> hPutStrLn stderr "Failed to parse complete response"
        _ -> do
          let errMsg = fromMaybe "Unknown error" (KM.lookup "error" val >>= \v -> case v of {String s -> Just (T.unpack s); _ -> Nothing})
          hPutStrLn stderr $ "Slack API error getting URL: " ++ errMsg
      _ -> hPutStrLn stderr "Failed to parse URL response"
