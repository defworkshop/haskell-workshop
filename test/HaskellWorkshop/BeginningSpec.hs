module HaskellWorkshop.BeginningSpec (main, spec) where

import HaskellWorkshop.Beginning
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "doubleUs" $ do
    it "should double the nubmer" $ do
      doubleNumber 4 9 `shouldBe` 26

  describe "myLast" $ do

    it "should return the last number of the sequence" $ do
      myLast [1, 2, 3] `shouldBe` 3

    it "should return raise an exception on empty list" $ do
      myLast [] `shouldThrow` anyErrorCall

  describe "myButLast" $ do

    it "should return the number before last" $ do
      myButLast [1, 2, 3] `shouldBe` 2

    it "should return the number before last" $ do
      myButLastThroughReverse [1, 2, 3] `shouldBe` 2

    it "should return raise an exception on empty list" $ do
      myButLast [] `shouldThrow` anyErrorCall

  describe "getNthFirstImpl" $ do
    it "should get the element at index" $ do
      getNthFirstImpl [5, 6, 7] 1 `shouldBe` 5
      getNthFirstImpl [5, 6, 7] 2 `shouldBe` 6
      -- getNthFirstImpl2 [5, 6, 7] 5 `shouldThrow` anyErrorCall
