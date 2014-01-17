module HaskellWorkshop.Beginning () where

import Test.Hspec
import Data.List

-- Functions in Haskell consist of two parts: signature and body.
-- All function bodies should follow the type signature:

doubleNumber :: Integer -> Integer -> Integer
doubleNumber x y = x*2 + y*2

-- Problem 1
--
-- Write a funciton that will return the last element of a list.
--
-- There're many ways of doing that. Some of examples:
--   * taking a `head` of the `reverse`d sequence
--   * recursively taking tail of the array until only one element's left, returning
--     that element. This could be done by using pattern matching.
--   * accessing last element by index (via `!!`)
--

myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs
myLast [] = error "empty list"

problem1Spec :: Spec
problem1Spec = do
  describe "myLast" $ do
    it "returns the last element of the list" $ do
      myLast [1,2,3,4] `shouldBe` 4
    it "returns the last element of the list" $ do
      myLast [1] `shouldBe` 1

-- Problem 2
-- Find the last but one element of a list.
--
-- You can do it via:
--   * getting the elements but last (`init`) and getting `last` element of that seq
--   * recursively, through pattern matchign
--   * through reversed sequence

myButLast :: [a] -> a
myButLast (x:(_:[])) = x
myButLast (_:xs) = myButLast xs
myButLast [] = error "empty list"

problem2Spec :: Spec
problem2Spec = do
  describe "myButLast" $ do
    it "returns last but one element" $ do
      myButLast [1,2,3,4] `shouldBe` 3


-- Problem 3
-- Find the K'th element of a list. The first element in the list is number 1.
--

elementAt :: [a] -> Int -> a
elementAt list k = list !! (k-1)

-- Problem 4
-- Find the number of elements of a list.

myLength           :: [a] -> Int
myLength []        =  0
myLength (_:xs)    =  1 + myLength xs

-- Problem 5
-- Reverse a list.

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = reverse xs ++ [x]

-- Problem 6
-- Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
--

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

-- Problem 7
-- Flatten a nested list structure.
--

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

-- flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])

-- Problem 8
-- Eliminate consecutive duplicates of list elements.
--
-- If a list contains repeated elements they should be replaced with a single copy of
-- the element. The order of the elements should not be changed.
--

compress :: Eq a => [a] -> [a]
compress (x:ys@(y:_))
    | x == y    = compress ys
    | otherwise = x : compress ys
compress ys = ys


-- Problem 9
-- Pack consecutive duplicates of list elements into sublists. If a list contains repeated
-- elements they should be placed in separate sublists.
--

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = if x `elem` (head (pack xs))
              then (x:(head (pack xs))):(tail (pack xs))
              else [x]:(pack xs)

-- Problem 10
-- Run-length encoding of a list. Use the result of problem P09 to implement the so-called
-- run-length encoding data compression method. Consecutive duplicates of elements are
-- encoded as lists (N E) where N is the number of duplicates of the element E.
--

encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . group
