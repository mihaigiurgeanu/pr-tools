module PRTools.FixParser
  ( parseBlock
  ) where

import Data.List (findIndex, intercalate, isPrefixOf)
import Data.List.Extra (trim)
import PRTools.ReviewState (Cmt(..))

-- | Parses a block of text representing a single comment from an edited source file
-- | during a fix session. It updates a list of existing comments with any changes
-- | found in the block.
parseBlock :: [String] -> [Cmt] -> [Cmt]
parseBlock (header:body) accCmts =
  let cidStart = length ("-- REVIEW COMMENT BEGIN [" :: String)
      cidEnd = findIndex (== ']') (drop cidStart header)
      cid = maybe "" (\end -> take end (drop cidStart header)) cidEnd
      afterCid = maybe "" (\end -> drop (cidStart + end + 1) header) cidEnd
      statusStart = findSub "[status:" afterCid
      afterStatusLabel = maybe afterCid (\start -> drop (start + 8) afterCid) statusStart
      statusEnd = findIndex (== ']') afterStatusLabel
      status = maybe "not-solved" (\end -> take end afterStatusLabel) statusEnd
      afterStatus = maybe afterStatusLabel (\end -> drop (end + 1) afterStatusLabel) statusEnd
      answerStart = findSub "[answer:" afterStatus

      headerAnswer = case answerStart of
        Nothing -> Nothing
        Just start ->
          let answerWithBracket = drop (start + 8) afterStatus
              answerEnd = findIndex (== ']') answerWithBracket
          in Just $ maybe answerWithBracket (\end -> take end answerWithBracket) answerEnd

      (textLines, answerLines) = span (not . ("[answer:" `isPrefixOf`)) body
      text = intercalate "\n" (map trim textLines)
      bodyAnswer =
        if null answerLines
        then Nothing
        else Just (intercalate "\n" (map trim (tail answerLines)))  -- Skip the [answer: line itself

      finalAnswer = case (headerAnswer, bodyAnswer) of
        (Just ha, Just ba) -> Just (ha ++ "\n" ++ ba)
        (Just ha, Nothing) -> Just ha
        (Nothing, Just ba) -> Just ba
        _ -> Nothing

      updated =
        map
          (\c ->
             if cmId c == cid
             then c { cmText = if null text then cmText c else text
                    , cmStatus = status
                    , cmAnswer = finalAnswer
                    }
             else c)
          accCmts
  in updated
parseBlock _ accCmts = accCmts

findSub :: String -> String -> Maybe Int
findSub sub str = go 0 str
  where
    go _ [] = Nothing
    go i xs = if sub `isPrefixOf` xs then Just i else go (i+1) (tail xs)
