module PRStateSpec (spec) where

import Test.Hspec
import PRTools.PRState
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
  describe "PRState event tracking" $ do
    it "should correctly add a review event" $ do
      let branch = "feature-branch"
      let reviewer = "John Doe"
      let action = "start"
      let pr = PRState "open" [] [] [] [] Nothing
      let state = Map.singleton branch pr
      
      let currentTime = "2025-08-12 10:00:00"
      let newEvent = ReviewEvent reviewer action currentTime
      let updatedPr = pr { prReviews = [newEvent] }
      let newState = Map.insert branch updatedPr state
      
      let retrievedPr = newState Map.! branch
      prReviews retrievedPr `shouldBe` [newEvent]

    it "should correctly add a fix event" $ do
      let branch = "feature-branch"
      let fixer = "Jane Doe"
      let action = "start"
      let pr = PRState "open" [] [] [] [] Nothing
      let state = Map.singleton branch pr
      
      let currentTime = "2025-08-12 11:00:00"
      let newEvent = FixEvent fixer action currentTime
      let updatedPr = pr { prFixes = [newEvent] }
      let newState = Map.insert branch updatedPr state
      
      let retrievedPr = newState Map.! branch
      prFixes retrievedPr `shouldBe` [newEvent]

    it "should correctly add an approval" $ do
      let branch = "feature-branch"
      let approver = "Jane Smith"
      let commits = [CommitInfo "hash1" "commit1", CommitInfo "hash2" "commit2"]
      let pr = PRState "open" [] [] [] [] Nothing
      let state = Map.singleton branch pr

      let currentTime = "2025-08-12 12:00:00"
      let newApproval = Approval approver currentTime commits
      let updatedPr = pr { approvalHistory = [newApproval] }
      let newState = Map.insert branch updatedPr state

      let retrievedPr = newState Map.! branch
      approvalHistory retrievedPr `shouldBe` [newApproval]

    it "should correctly record merge info" $ do
      let branch = "feature-branch"
      let merger = "Admin"
      let commits = [CommitInfo "hash1" "commit1", CommitInfo "hash2" "commit2"]
      let pr = PRState "open" [] [] [] [] Nothing
      let state = Map.singleton branch pr

      let currentTime = "2025-08-12 13:00:00"
      let mergeInfo = MergeInfo merger currentTime commits
      let updatedPr = pr { prStatus = "merged", prMergeInfo = Just mergeInfo }
      let newState = Map.insert branch updatedPr state

      let retrievedPr = newState Map.! branch
      prMergeInfo retrievedPr `shouldBe` Just mergeInfo
      prStatus retrievedPr `shouldBe` "merged"
