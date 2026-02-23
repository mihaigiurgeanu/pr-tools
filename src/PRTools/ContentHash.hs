module PRTools.ContentHash where

import Data.Hashable (hash)
import Data.List (isPrefixOf)
import System.Process (readProcess)
import PRTools.Config (trimTrailing)

-- Generate a hash from the patch content, excluding metadata
generatePatchHash :: String -> String -> IO String
generatePatchHash baseCommit headCommit = do
  patchContent <- readProcess "git" ["diff", baseCommit ++ ".." ++ headCommit] ""
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
  in unlines normalized

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
