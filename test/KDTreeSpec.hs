module KDTreeSpec where

import Test.Hspec
import KDTree
import Point1

main :: IO ()
main = hspec spec

pm = Point1 (-1)
p = Point1 0
pp = Point1 1
ps :: [Point1 Int]
ps = [pm, p, pp]

pq = Point1 99

kd :: KDTree (Point1 Int)
kd = fromList ps

spec :: Spec
spec =  do
  describe "KDTree" $ do
    it "depth of empty tree is 0" $
      depth Nil `shouldBe` 0

    it "depth of a leaf is 1" $
      depth (Leaf 2) `shouldBe` 1

    it "depth of a node is depth of child + 1" $
      depth (Node 0 100 (Leaf 50) Nil (Leaf 100)) `shouldBe` 2

    it "be an empty tree if created from empty list" $
      fromList ([]::[Point1 Int]) `shouldBe` Nil

    it "be a tree with child nodes if created from list" $
      kd `shouldBe` (Node 1 0 (Leaf (-1)) (Leaf 0) (Leaf 1))

    it "should find members in tree" $
      member p kd `shouldBe` True

    it "should not find points not in tree" $
      member pq kd `shouldBe` False

    it "should find the points in a range" $
      rangeQuery kd (pm, p) `shouldBe` [pm, p]
      