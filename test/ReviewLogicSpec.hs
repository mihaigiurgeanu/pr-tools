module ReviewLogicSpec (spec) where

import           PRTools.ReviewLogic (filterComments)
import           PRTools.ReviewState (Cmt (..))
import           Test.Hspec

spec :: Spec
spec = do
  describe "PRTools.ReviewLogic.filterComments" $ do
    let resolvedComment = Cmt "id1" "file.hs" 1 "resolved" True "solved" Nothing ""
    let unresolvedComment = Cmt "id2" "file.hs" 2 "unresolved" False "not-solved" Nothing ""
    let comments = [resolvedComment, unresolvedComment]

    it "returns all comments when showAll is True" $ do
      filterComments True comments `shouldBe` comments

    it "returns only unresolved comments when showAll is False" $ do
      filterComments False comments `shouldBe` [unresolvedComment]

    it "returns an empty list if all comments are resolved and showAll is False" $ do
      filterComments False [resolvedComment] `shouldBe` []

    it "returns all comments if all are unresolved and showAll is False" $ do
      filterComments False [unresolvedComment] `shouldBe` [unresolvedComment]

    it "returns an empty list for an empty input list" $ do
      filterComments True ([] :: [Cmt]) `shouldBe` []
      filterComments False ([] :: [Cmt]) `shouldBe` []
