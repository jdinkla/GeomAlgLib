module Point2Spec where

import Test.Hspec
import Point2

main :: IO ()
main = hspec spec

spec :: Spec
spec =  do
  describe "Point2" $ do
    it "dimension is 2" $
      dimension (Point2 (10, 11)) `shouldBe` 2

    it "origin is zero" $
      (origin :: Point2 Int) `shouldBe` (Point2 (0,0))

    it "1st coordinate" $
      ith 1 (Point2 (5,7)) `shouldBe` 5

    it "2nd coordinate" $
      ith 2 (Point2 (5,7)) `shouldBe` 7
