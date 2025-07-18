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
    recBuild :: Int -> [PolyDiff [String] [String]] -> [(Maybe Int, String)]
    recBuild _ [] = []
    recBuild n (g:gs) = case g of
      Both ls _ ->
        let feature_lines = zip [n ..] ls
            annotated = map (\(l, line) -> (Just l, line)) feature_lines
        in annotated ++ recBuild (n + length ls) gs
      First ls -> case gs of
        (Second ms : rest) ->
          let base_lines = map (Nothing ,) ls
              feature_lines = zip [n ..] ms
              annotated = map (\(l, line) -> (Just l, line)) feature_lines
          in (Nothing, "<<<<<<< BASE") : base_lines ++ (Nothing, "=======") : annotated ++ (Nothing, ">>>>>>> FEATURE") : recBuild (n + length ms) rest
        _ ->
          let base_lines = map (Nothing ,) ls
          in (Nothing, "<<<<<<< BASE") : base_lines ++ (Nothing, "=======") : (Nothing, ">>>>>>> FEATURE") : recBuild n gs
      Second ms ->
        let feature_lines = zip [n ..] ms
            annotated = map (\(l, line) -> (Just l, line)) feature_lines
        in (Nothing, "<<<<<<< BASE") : (Nothing, "=======") : annotated ++ (Nothing, ">>>>>>> FEATURE") : recBuild (n + length ms) gs

    insertComments :: [(Maybe Int, String)] -> [Cmt] -> [String]
    insertComments clines scmts =
      let go (rev_acc, rem_cmts) (mb_l, line) =
            case mb_l of
              Nothing -> (line : rev_acc, rem_cmts)
              Just l ->
                let (inserted, remaining) = span (\c -> cmLine c == l) rem_cmts
                    markers = map (\c -> "-- COMMENT [" ++ cmId c ++ "]: " ++ trimTrailingNewlines (cmText c)) inserted
                    to_add = markers ++ [line]
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
  let (augmentedLines, _) = foldl' (\(acc, offset) c ->
                                      let insert_pos = cmLine c + offset
                                          before = take insert_pos acc
                                          after = drop insert_pos acc
                                          marker = "-- REVIEW COMMENT [" ++ cmId c ++ "]: " ++ cmText c ++ " [status:" ++ cmStatus c ++ "] [answer:" ++ fromMaybe "" (cmAnswer c) ++ "]"
                                      in (before ++ [marker] ++ after, offset + 1)
                                   ) (fileLines, 0) sortedCmts
  return $ unlines augmentedLines
