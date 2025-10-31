module FixParserSpec (spec) where

import Test.Hspec
import PRTools.ReviewState (Cmt(..))
import PRTools.FixParser (parseBlock)

spec :: Spec
spec = do
  describe "PRTools.FixParser.parseBlock" $ do
    it "should correctly parse an answer without the trailing square bracket" $ do
      let initialComment = Cmt "id123" "file.hs" 1 "text" False "not-solved" Nothing ""
      let comments = [initialComment]
      -- THIS IS THE CORRECTED LINE:
      let header = "-- REVIEW COMMENT BEGIN [id123] [status:solved] [answer:My answer is here]]"
      let block = [header]

      let updatedComments = parseBlock block comments
      let resultComment = head updatedComments
      cmAnswer resultComment `shouldBe` Just "My answer is here"
