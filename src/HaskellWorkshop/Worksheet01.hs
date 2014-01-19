module Main
       (
         main
       ) where

import Test.Hspec


--------------------------------------------------------------------------------
-- Problem 1
--
-- Write a function that returns the lesser of two Ints.
--------------------------------------------------------------------------------
myMin :: Int -> Int -> Int
myMin = undefined

myMinSpec :: Spec
myMinSpec = do
  describe "myMin" $ do
    it "returns the lesser of two Ints" $ do
      myMin (-1) 1 `shouldBe` (-1)
      myMin 1 (-1) `shouldBe` (-1)


--------------------------------------------------------------------------------
-- Problem 2
--
-- Write a function that takes a triple of integers  and checks if they can form
-- the sides of a right triangle, such that the last element is the hypothenuse.
--------------------------------------------------------------------------------
isRightTriangle :: (Int, Int, Int) -> Bool
isRightTriangle (x,y,z) = undefined

isRightTriangleSpec :: Spec
isRightTriangleSpec = do
  describe "isRightTriangle" $ do
    it "accepts triples where the last could be the hypothenuse" $ do
      isRightTriangle (3,4,5) `shouldBe` True

    it "rejects triples where not the last entry could be the hypothenuse" $ do
      isRightTriangle (3,5,4) `shouldBe` False
      isRightTriangle (3,4,6) `shouldBe` False


--------------------------------------------------------------------------------
-- Problem 3
--
-- Write a function that takes a list of triples of integers and returns true if
-- any of those triples can form a right triangle
--------------------------------------------------------------------------------
anyRightTriangle :: [(Int,Int,Int)] -> Bool
anyRightTriangle = undefined

anyRightTriangleSpec :: Spec
anyRightTriangleSpec = do
  describe "anyRightTriangle" $ do
    it "rejects the empty list" $ do
      anyRightTriangle [] `shouldBe` False

    it "rejects lists which consist of triples unacceptable by 'isRightTriangle'" $ do
      anyRightTriangle [(3,5,4)] `shouldBe` False
      anyRightTriangle [(3,4,6)] `shouldBe` False
      anyRightTriangle [(3,5,4),(3,4,6)] `shouldBe` False

    it "accepts lists which contain at least one triple acceptable by 'isRightTriangle'" $ do
      anyRightTriangle [(3,5,4),(3,4,6),(3,4,5)] `shouldBe` True


--------------------------------------------------------------------------------
-- Problem 4
--
-- Write a function that will return the last element of a list.
-- Return 'Nothing' if there is no last element
--------------------------------------------------------------------------------
myLast :: [a] -> Maybe a
myLast = undefined

myLastSpec :: Spec
myLastSpec = do
  describe "myLast" $ do
    it "returns the last element of the list" $ do
      myLast [1,2,3,4] `shouldBe` Just (4::Int)
      myLast [1] `shouldBe` Just (1::Int)

    it "returns Nothing on an empty list" $ do
      myLast [] `shouldBe` (Nothing :: Maybe Int)


--------------------------------------------------------------------------------
-- Problem 5
-- Find the K'th element of a list. The first element in the list is number 1.
-- Don't use the `!!` list index operator
--------------------------------------------------------------------------------
elementAt :: [a] -> Int -> Maybe a
elementAt = undefined

elementAtSpec :: Spec
elementAtSpec = do
  describe "elementAt" $ do
    it "returns the k-th element if there are that many" $ do
      elementAt [1,2,3] 1 `shouldBe` Just (1 :: Int)
      elementAt [1,2,3] 3 `shouldBe` Just (3 :: Int)
      elementAt [1..100] 99 `shouldBe` Just (99 :: Int)


--------------------------------------------------------------------------------
-- Problem 6
-- Find the number of elements of a list. Can you write a tail recursive version?
--------------------------------------------------------------------------------
myLength :: [a] -> Int
myLength = undefined

myLengthSpec :: Spec
myLengthSpec = do
  describe "myLength" $ do
    it "measures the length of a list" $ do
      myLength [] ` shouldBe` 0
      myLength [1,2,(3::Int)] `shouldBe` 3
      myLength [1..(100::Int)] `shouldBe` 100


--------------------------------------------------------------------------------
-- Problem 7
-- Reverse a list.
--------------------------------------------------------------------------------
myReverse :: [a] -> [a]
myReverse = undefined

myReverseSpec :: Spec
myReverseSpec = do
  describe "myReverse" $ do
    it "gives an empty list when given an empty list" $ do
      myReverse [] `shouldBe` ([] :: [Int])
      myReverse [1,2,3] `shouldBe` [3,2,1::Int]


--------------------------------------------------------------------------------
-- Problem 8
-- Find out whether a list is a palindrome. A palindrome can be read forward or
-- backward; e.g. (x a m a x).
--------------------------------------------------------------------------------
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome = undefined

isPalindromeSpec :: Spec
isPalindromeSpec = do
  describe "isPalindrome" $ do
    it "accepts the empty list" $ do
      isPalindrome "" `shouldBe` True

    it "accepts some simple palindromes" $ do
      isPalindrome [1,2,3,2,1::Int] `shouldBe` True
      isPalindrome "ANNA" `shouldBe` True
      isPalindrome "oTTo" `shouldBe` True

    it "rejects lists that are not palindromes" $ do
      isPalindrome "abc" `shouldBe` False
      isPalindrome "abbac" `shouldBe` False


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
