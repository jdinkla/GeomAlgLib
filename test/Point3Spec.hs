module Point3Spec where

import Test.Hspec
import Point3

main :: IO ()
main = hspec spec

spec :: Spec
spec =  do
  describe "Point3" $ do
    it "dimension is 3" $
      dimension (Point3 (10, 11, 12)) `shouldBe` 3

    it "origin is zero" $
      (origin :: Point3 Int) `shouldBe` (Point3 (0,0,0))

    it "1st coordinate" $
      ith 1 (Point3 (5,7,9)) `shouldBe` 5

    it "2nd coordinate" $
      ith 2 (Point3 (5,7,9)) `shouldBe` 7

    it "3rd coordinate" $
      ith 3 (Point3 (5,7,9)) `shouldBe` 9
      
