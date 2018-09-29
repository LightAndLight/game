module Main where

import Test.Hspec
import Numeric.Interval
import AABBTree (BB(..), Tree(..), treeBB)

import qualified Numeric.Interval as Interval
import qualified AABBTree as Tree

data X = A | B | C deriving (Eq, Show)

main :: IO ()
main = hspec $ do
  describe "AABBTree" $ do
    let
      bbA :: BB Float
      bbA = BB (0 ... 2) (4 ... 6)

      bbB :: BB Float
      bbB = BB (8.5 ... 9.5) (2.5 ... 3.5)

      bbC :: BB Float
      bbC = BB (9 ... 10) (3 ... 4)

      tree1 = Tree.insert (A, bbA) Tree.empty
      tree2 = Tree.insert (B, bbB) tree1
      tree3 = Tree.insert (C, bbC) tree2

    it "intersection works" $ do
      Tree.bbIntersects bbA bbB `shouldBe` False
      Tree.bbIntersects bbA bbC `shouldBe` False
      Tree.bbIntersects bbB bbC `shouldBe` True

    it "insert works" $ do
      Tree.bbIntersects bbA (treeBB tree1) `shouldBe` True
      Tree.bbIntersects bbA (treeBB tree2) `shouldBe` True
      Tree.bbIntersects bbA (treeBB tree3) `shouldBe` True

      Tree.bbIntersects bbB (treeBB tree1) `shouldBe` False
      Tree.bbIntersects bbB (treeBB tree2) `shouldBe` True
      Tree.bbIntersects bbB (treeBB tree3) `shouldBe` True

      Tree.bbIntersects bbC (treeBB tree1) `shouldBe` False
      Tree.bbIntersects bbC (treeBB tree3) `shouldBe` True
      Tree.bbIntersects bbC (treeBB tree3) `shouldBe` True

    it "collisions works" $ do
      Tree.collisions tree1 `shouldBe` []
      Tree.collisions tree2 `shouldBe` []
      Tree.collisions tree3 `shouldBe` [(C, B)]
