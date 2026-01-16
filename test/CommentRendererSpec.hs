module CommentRendererSpec where

import Test.Hspec
import PRTools.CommentRenderer (detectEol, normalizeLines, joinLines)

spec :: Spec
spec = do
  describe "EOL handling" $ do
    it "detects CRLF and preserves it in joinLines" $ do
      let content = "a\r\nb\r\n"
      detectEol content `shouldBe` "\r\n"
      normalizeLines content `shouldBe` ["a", "b"]
      joinLines "\r\n" ["a", "b"] `shouldBe` "a\r\nb\r\n"

    it "defaults to LF when no CRLF is present" $ do
      let content = "a\nb\n"
      detectEol content `shouldBe` "\n"
      normalizeLines content `shouldBe` ["a", "b"]
      joinLines "\n" ["a", "b"] `shouldBe` "a\nb\n"
