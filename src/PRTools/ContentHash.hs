module PRTools.ContentHash where

import Data.Hashable (hash)
import Data.List (isPrefixOf)
import System.Process (readProcess)
import PRTools.Config (trimTrailing)

-- Generate a hash from the patch content, excluding metadata
-- Uses the merge-base to ensure consistent comparison point
generatePatchHash :: String -> String -> IO String
generatePatchHash baseCommit headCommit = do
  -- Find the actual merge base between the base branch and the commit
  mergeBase <- trimTrailing <$> readProcess "git" ["merge-base", baseCommit, headCommit] ""
  patchContent <- readProcess "git" ["diff", mergeBase, headCommit] ""
  let normalizedPatch = normalizePatch patchContent
  return $ take 12 $ show $ abs $ hash normalizedPatch

-- Normalize patch content by removing line numbers and normalizing whitespace
normalizePatch :: String -> String
normalizePatch patch = 
  let patchLines = lines patch
      -- Remove @@ line number headers and file headers, keep only actual changes
      contentLines = filter (not . isPatchHeader) patchLines
      -- Normalize whitespace but preserve structure
      normalized = map normalizeWhitespace contentLines
      -- Remove empty lines that might vary between diffs
      nonEmpty = filter (not . null . trim) normalized
  in unlines nonEmpty
  where
    trim = dropWhile (`elem` " \t") . reverse . dropWhile (`elem` " \t") . reverse

-- Check if a line is a patch header that should be ignored for content comparison
isPatchHeader :: String -> Bool
isPatchHeader line = 
  "@@" `isPrefixOf` line ||
  "diff --git" `isPrefixOf` line ||
  "index " `isPrefixOf` line ||
  "--- " `isPrefixOf` line ||
  "+++ " `isPrefixOf` line

-- Normalize whitespace while preserving the +/- prefix structure
normalizeWhitespace :: String -> String
normalizeWhitespace line
  | "+" `isPrefixOf` line = "+" ++ (unwords . words . drop 1) line
  | "-" `isPrefixOf` line = "-" ++ (unwords . words . drop 1) line
  | otherwise = unwords . words $ line

-- Debug function to compare patch content (for troubleshooting)
debugPatchDifference :: String -> String -> String -> IO ()
debugPatchDifference baseCommit oldCommit newCommit = do
  -- Use merge-base for consistent comparison
  oldMergeBase <- trimTrailing <$> readProcess "git" ["merge-base", baseCommit, oldCommit] ""
  newMergeBase <- trimTrailing <$> readProcess "git" ["merge-base", baseCommit, newCommit] ""
  
  putStrLn $ "Old merge base: " ++ oldMergeBase
  putStrLn $ "New merge base: " ++ newMergeBase
  
  oldPatch <- readProcess "git" ["diff", oldMergeBase, oldCommit] ""
  newPatch <- readProcess "git" ["diff", newMergeBase, newCommit] ""
  let oldNormalized = normalizePatch oldPatch
  let newNormalized = normalizePatch newPatch
  putStrLn "=== OLD PATCH (normalized) ==="
  putStrLn oldNormalized
  putStrLn "=== NEW PATCH (normalized) ==="
  putStrLn newNormalized
  putStrLn "=== PATCHES EQUAL? ==="
  putStrLn $ show (oldNormalized == newNormalized)
