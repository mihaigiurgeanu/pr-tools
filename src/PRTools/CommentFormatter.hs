module PRTools.CommentFormatter where

import Data.Char (isSpace)
import Data.List (findIndex, intercalate, isPrefixOf)
import Data.List.Extra (trim)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, catMaybes, listToMaybe)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import PRTools.ReviewState (Cmt(..))
import System.IO (hPutStrLn, stderr)

formatComment :: Cmt -> String
formatComment c =
  unlines [ "File: " ++ cmFile c
          , "Line: " ++ show (cmLine c)
          , "ID: " ++ cmId c
          , "Status: " ++ cmStatus c
          , "Resolved: " ++ show (cmResolved c)
          , "Revision: " ++ cmRevision c
          , "Comment:"
          ] ++ cmText c ++ "\n" ++
  (case cmAnswer c of
     Just a | not (all isSpace a) -> "Answer: " ++ a ++ "\n"
     _ -> []) ++
  "---"

parseSection :: String -> IO (Maybe Cmt)
parseSection s = do
  let ls = lines s
  let findKeyIdx key = findIndex (\ln -> key `isPrefixOf` trim ln) ls
  let getValue :: Int -> String -> Maybe String
      getValue idx key = if idx < 0 then Nothing else let ln = ls !! idx in Just (drop (length key) ln)
  let fileIdx = findKeyIdx "File:"
  let lineIdx = findKeyIdx "Line:"
  let idIdx = findKeyIdx "ID:"
  let statusIdx = findKeyIdx "Status:"
  let resolvedIdx = findKeyIdx "Resolved:"
  let revisionIdx = findKeyIdx "Revision:"
  let commentIdx = findKeyIdx "Comment:"
  let answerIdx = findKeyIdx "Answer:"
  case (fileIdx, lineIdx, idIdx, statusIdx, commentIdx) of
    (Just fIdx, Just lIdx, Just iIdx, Just sIdx, Just cIdx) -> do
      let fileM = trim <$> getValue fIdx "File:"
      let lineStrM = trim <$> getValue lIdx "Line:"
      let lineM = lineStrM >>= \str -> case reads str of {[(l, "")] -> Just l; _ -> Nothing}
      let cidM = trim <$> getValue iIdx "ID:"
      let statusM = trim <$> getValue sIdx "Status:"
      let resolvedM = resolvedIdx >>= (\rIdx -> trim <$> getValue rIdx "Resolved:") >>= \str -> Just (str == "True")
      let revisionM = revisionIdx >>= (\rIdx -> trim <$> getValue rIdx "Revision:")
      case (fileM, lineM, cidM) of
        (Just file, Just line, Just cid) | not (null file) && not (null cid) -> do
          let status = fromMaybe "not-solved" statusM
          let resolved = fromMaybe False resolvedM
          let revision = fromMaybe "" revisionM
          let commentStart = getValue cIdx "Comment:"
          let commentEndIdx = fromMaybe (length ls) answerIdx
          let commentRest = take (commentEndIdx - cIdx - 1) (drop (cIdx + 1) ls)
          let commentParts = maybe [] (: commentRest) commentStart
          let commentText = intercalate "\n" (map trim commentParts)
          let answerStart = answerIdx >>= ( `getValue` "Answer:")
          let answerRest = drop (commentEndIdx + 1) ls
          let answerParts = maybe [] (: answerRest) answerStart
          let answerText = intercalate "\n" (map trim answerParts)
          let finalAnswer = if all isSpace answerText then Nothing else Just answerText
          u <- if null cid then nextRandom else return undefined
          let finalId = if null cid then take 8 $ filter (/= '-') $ toString u else cid
          return $ Just $ Cmt finalId file line commentText resolved status finalAnswer revision
        _ -> putStrLn "Section skipped: missing or invalid required fields." >> return Nothing
    _ -> putStrLn "Section skipped: missing or misordered fields." >> return Nothing

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
