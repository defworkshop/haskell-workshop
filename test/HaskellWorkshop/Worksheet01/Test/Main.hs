module Main where

import HaskellWorkshop.Worksheet01
import Test.Hspec


main :: IO ()
main = hspec $ do
  myMinSpec
  isRightTriangleSpec
  anyRightTriangleSpec
  myLastSpec
  elementAtSpec
  myLengthSpec
  myReverseSpec
  isPalindromeSpec
