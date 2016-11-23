module Point1Spec where

import Test.Hspec
import Point1 

main :: IO ()
main = hspec spec

spec :: Spec
spec =  do
  describe "Point1" $ do
    it "dimension is 1" $
      dimension (Point1 10) `shouldBe` 1

    it "origin is zero" $
      (origin :: Point1 Int) `shouldBe` (Point1 0)

    it "ith coordinate" $
      ith 1 (Point1 10) `shouldBe` 10
