module PRTools.ReviewLogic
  ( filterComments
  ) where

import           PRTools.ReviewState (Cmt (..))

-- | Filters comments based on the showAll flag.
-- If showAll is False, only unresolved comments are returned.
filterComments :: Bool -> [Cmt] -> [Cmt]
filterComments showAll = if showAll then id else filter (not . cmResolved)
