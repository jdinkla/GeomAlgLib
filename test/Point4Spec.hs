module Point4Spec where

import Test.Hspec
import Point4

main :: IO ()
main = hspec spec

p0 :: Point4 Int
p0 = Point4 (0,0,0,0)

p1 :: Point4 Int
p1 = Point4 (5,7,9,11)
  
spec :: Spec
spec =  do
  describe "Point4" $ do
    it "dimension is 4" $
      dimension p1 `shouldBe` 4

    it "origin is zero" $
      (origin :: Point4 Int) `shouldBe` p0

    it "1st coordinate" $
      ith 1 p1 `shouldBe` 5

    it "2nd coordinate" $
      ith 2 p1 `shouldBe` 7

    it "3rd coordinate" $
      ith 3 p1 `shouldBe` 9
      
    it "4th coordinate" $
      ith 4 p1 `shouldBe` 11
