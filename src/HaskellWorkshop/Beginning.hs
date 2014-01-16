module HaskellWorkshop.Beginning (doubleNumber,
                                  myLast,
                                  myButLast,
                                  myButLastThroughReverse,
                                  getNthFirstImpl,
                                  getNthFirstImpl2) where

-- import Insanity.Beginning.Internal

doubleNumber :: Integer -> Integer -> Integer
doubleNumber x y = x*2 + y*2

-- Problem 1

myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs
myLast [] = error "empty list"

-- Problem 2

myButLast :: [a] -> a
myButLast (x:(_:[])) = x
myButLast (_:xs) = myButLast xs
myButLast [] = error "empty list"


myButLastThroughReverse :: [a] -> a
-- myButLastThroughReverse x = head(tail(reverse(x)))
myButLastThroughReverse = head . tail . reverse


-- Problem 3

getNthFirstImpl :: [a] -> Int -> a
getNthFirstImpl arr i = arr !! (i - 1)

getNthFirstImpl2 :: [a] -> Int -> a
getNthFirstImpl2 (x:_) 1 = x
getNthFirstImpl2 (_:xs) n = getNthFirstImpl2 xs (n-1)
getNthFirstImpl2 _ _ = error "Out of bounds"
