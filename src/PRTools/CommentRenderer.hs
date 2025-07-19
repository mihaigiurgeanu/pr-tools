{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PRTools.CommentRenderer where

import Control.Exception (catch, IOException)
import Data.Algorithm.Diff (PolyDiff(..), getGroupedDiff)
import Data.List (foldl', sortBy)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess)
import PRTools.ReviewState (Cmt(..))
import Data.Maybe (fromMaybe, catMaybes)
import Data.Ord (comparing)
import Data.List (dropWhile, span)
import PRTools.Config (trimTrailing)
import Control.Arrow (first)

-- Remap a line number from old revision to current
remapLine :: String -> String -> String -> Int -> IO Int
remapLine revision file branch target = do
  oldContent <- catch (readProcess "git" ["show", revision ++ ":" ++ file] "")
                      (\e -> do hPutStrLn stderr $ "Warning: " ++ show (e :: IOException); return "")
  currentContent <- readProcess "git" ["show", branch ++ ":" ++ file] ""
  let oldLines = lines oldContent
  let currentLines = lines currentContent
  let diffs = getGroupedDiff oldLines currentLines
  return $ go diffs 1 1 target
  where
    go [] o n t = n + (t - o)
    go (d:ds) o n t = case d of
        Both ls _ ->
          let size = length ls
          in if o + size > t then n + (t - o)
             else go ds (o + size) (n + size) t
        First ls ->
          let size = length ls
          in if o + size > t then n
             else go ds (o + size) n t
        Second ls ->
          let size = length ls
          in go ds o (n + size) t

-- Render for review: conflict style with comments inserted
renderForReview :: String -> String -> String -> [Cmt] -> IO String
renderForReview baseB branch file cmts = do
  baseContent <- catch (readProcess "git" ["show", baseB ++ ":" ++ file] "")
                       (\(_ :: IOException) -> return "")
  featureContent <- catch (readProcess "git" ["show", branch ++ ":" ++ file] "")
                          (\e -> do hPutStrLn stderr $ "Warning: " ++ show (e :: IOException); return "")
  currentRev <- trimTrailing <$> readProcess "git" ["rev-parse", branch] ""
  let baseLines = lines baseContent
  let featureLines = lines featureContent
  adjustedCmts <- mapM (\c -> do
    newLine <- if cmRevision c == currentRev then return (cmLine c) else remapLine (cmRevision c) (cmFile c) branch (cmLine c)
    return c { cmLine = newLine }
    ) cmts
  let sortedCmts = sortBy (comparing cmLine) adjustedCmts
  let groups = getGroupedDiff baseLines featureLines
  let conflictLines = recBuild 1 groups
  let augmented = insertComments conflictLines sortedCmts
  return $ unlines augmented
  where
    annotateFeatureLines :: Int -> [String] -> [(Maybe Int, String)]
    annotateFeatureLines n ls = let feature_lines = zip [n ..] ls
                                in map (first Just) feature_lines

    recBuild :: Int -> [PolyDiff [String] [String]] -> [(Maybe Int, String)]
    recBuild _ [] = []
    recBuild n (g:gs) = case g of
      Both ls _ ->
        let annotated = annotateFeatureLines n ls
        in annotated ++ recBuild (n + length ls) gs
      First ls ->
        let base_lines = map (Nothing ,) ls
        in case gs of
          (Second ms : rest) ->
            let annotated = annotateFeatureLines n ms
            in (Nothing, "<<<<<<< BASE") : base_lines ++ (Nothing, "=======") : annotated ++ (Nothing, ">>>>>>> FEATURE") : recBuild (n + length ms) rest
          _ ->
            (Nothing, "<<<<<<< BASE") : base_lines ++ (Nothing, "=======") : (Nothing, ">>>>>>> FEATURE") : recBuild n gs
      Second ms ->
        let annotated = annotateFeatureLines n ms
        in (Nothing, "<<<<<<< BASE") : (Nothing, "=======") : annotated ++ (Nothing, ">>>>>>> FEATURE") : recBuild (n + length ms) gs

    insertComments :: [(Maybe Int, String)] -> [Cmt] -> [String]
    insertComments clines scmts =
      let go (rev_acc, rem_cmts) (mb_l, line) =
            case mb_l of
              Nothing -> (line : rev_acc, rem_cmts)
              Just l ->
                let (inserted, remaining) = span (\c -> cmLine c == l) rem_cmts
                    markers = map (\c -> "-- COMMENT [" ++ cmId c ++ "]: " ++ trimTrailingNewlines (cmText c)) inserted
                    to_add = line : markers
                    new_rev = foldl' (flip (:)) rev_acc (reverse to_add)
                in (new_rev, remaining)
          (final_rev, final_rem) = foldl' go ([], scmts) clines
          augmented = reverse final_rev
          final_aug = if null final_rem then augmented else augmented ++ map (\c -> "-- COMMENT [" ++ cmId c ++ "]: " ++ trimTrailingNewlines (cmText c)) final_rem
      in final_aug
      where
        trimTrailingNewlines s = reverse $ dropWhile (== '\n') $ reverse s

-- Render for fix: current content with editable markers
renderForFix :: String -> String -> [Cmt] -> IO String
renderForFix branch file cmts = do
  content <- catch (readProcess "git" ["show", branch ++ ":" ++ file] "")
                   (\e -> do hPutStrLn stderr $ "Warning: " ++ show (e :: IOException); return "")
  currentRev <- trimTrailing <$> readProcess "git" ["rev-parse", branch] ""
  let fileLines = lines content
  adjustedCmts <- mapM (\c -> do
    newLine <- if cmRevision c == currentRev then return (cmLine c) else remapLine (cmRevision c) (cmFile c) branch (cmLine c)
    return c { cmLine = newLine }
    ) cmts
  let sortedCmts = sortBy (comparing cmLine) adjustedCmts
  let (augmentedLines, _) = foldl' insertComment (fileLines, 0) sortedCmts
  return $ unlines augmentedLines
  where
    insertComment :: ([String], Int) -> Cmt -> ([String], Int)
    insertComment (acc, offset) c =
      let insert_pos' = cmLine c + offset - 1
          insert_pos = max 0 insert_pos'
          before = take insert_pos acc
          after = drop insert_pos acc
          marker = "-- REVIEW COMMENT [" ++ cmId c ++ "]: " ++ cmText c ++ " [status:" ++ cmStatus c ++ "] [answer:" ++ fromMaybe "" (cmAnswer c) ++ "]"
      in (before ++ [marker] ++ after, offset + 1)

displayComments :: String -> [Cmt] -> Bool -> IO ()
displayComments branch cmts withCtx = do
  if withCtx then
    mapM_ (\c -> do
      content <- readProcess "git" ["show", branch ++ ":" ++ cmFile c] ""
      let fileLines = lines content
      let start = max 0 (cmLine c - 4)
      let context = take 7 (drop start fileLines)
      let numberedContext = zipWith (\i ln -> "  " ++ show (start + 1 + i) ++ ": " ++ ln) [0..] context
      putStrLn $ "File: " ++ cmFile c ++ "\nLine: " ++ show (cmLine c) ++ "\nID: " ++ cmId c ++ "\nStatus: " ++ cmStatus c ++ "\nComment: " ++ cmText c ++ "\nAnswer: " ++ fromMaybe "" (cmAnswer c) ++ "\nContext:\n" ++ unlines numberedContext ++ "\n---"
      ) cmts
    else
    mapM_ (\c -> putStrLn $ cmFile c ++ ":" ++ show (cmLine c) ++ " [" ++ cmId c ++ "] - " ++ map (\ch -> if ch == '\n' then ' ' else ch) (cmText c) ++ " [" ++ cmStatus c ++ "]" ++ maybe "" (\a -> " answer: " ++ map (\ch -> if ch == '\n' then ' ' else ch) a) (cmAnswer c)) cmts
