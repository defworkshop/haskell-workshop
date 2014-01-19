{-# LANGUAGE NoMonomorphismRestriction #-}


-- =========================================
-- Explaining IO in Haskell
-- =========================================

module HaskellWorkshop.WorksheetIO where

import Control.Monad

import Debug.Trace (trace)

-- =========================================
-- do-notation
-- =========================================


-- test1a: do-notation
test1a :: IO ()
test1a = do
  putStr "Type something: "
  line <- getLine
  putStrLn line


-- testb: desugared pointful version (explicit arguments)
test1b :: IO ()
test1b =
  putStr "Type something: " >> getLine >>= \line -> putStrLn line

-- test1c: desugared pointfree version (no explicit arguments)
test1c :: IO ()
test1c =
  putStr "Type something: " >> getLine >>= putStrLn

-- Questions
-- What are the types of getLine, putStr, putStrLn, (>>) and (>>=) ?
-- Hint: Use ghci's :t directive to find out types.
-- How are the left arrow "<-" and these two functions related?


-- =========================================
-- Type annotations
-- =========================================

-- Sometimes we have to use type annotations
-- to help the compiler.

readIntA :: IO ()
readIntA = do
  putStr "Type an integer: "
  x <- getLine
  putStrLn (show (read x :: Integer))

-- What happens if you don't use a type annotation? Why?
-- Try it out in ghc.

-- Compare to this:

readIntB :: IO Integer
readIntB = do
  putStr "Type an integer: "
  x <- getLine
  return (read x)

-- What happens if you forget the type signature of "readIntB"?
-- What's the difference between ommitting the type annotation
-- in readIntA and the type signature of readIntB?

-- Task:
-- Try to convert the functions readIntA and readIntB to their
-- desugared versions. Try first to make it pointfull, then pointfree.
-- Hint: Maybe you have to use function types in your type annotations.

-- Task:
-- Write a function that reads in two integers and calculates
-- the greatest common devisor. Use Euclides algorithm defined here:
-- http://en.wikipedia.org/wiki/Greatest_common_divisor#Using_Euclid.27s_algorithm
-- Dont forget the type signatures for your functions. It's good style!
-- Try to write the do-Version, the poinful and pointfree versions.
-- Hint: Your gcd function should have this type:

gcdA :: Integer -> Integer -> Integer
gcdA = undefined

-- =========================================
-- Lazy data structures
-- =========================================

-- Task:
-- Rewrite your gcd function to have this signature:

gcdB :: Integer -> Integer -> [(Integer, Integer)]
gcdB = undefined

-- You should prepend the two argments to each recursiv call.
-- Thus, you will get a list of intermediate results, the final
-- result being the last member of the list.
-- Try to use the function "last" and "fst" to get your final
-- result printed in ghci as before.

-- =========================================
-- IO-Actions
-- =========================================

-- Task:
-- Rewrite your gcd function to have this signature:

gcdC :: Integer -> Integer -> [IO ()]
gcdC = undefined

-- Apply print on each element of the list.
-- Try use this to print your trace:

printIt :: IO ()
printIt = sequence_ (gcdC 129280 232680)

-- Task:
-- Reverse this trace. Print only every second item of the trace.
-- How would you do this in imperative language
-- without modifying the algorithm itself?

-- Tasks:
-- Try "gcdB" and "sequence_ . map print" instead of "gcdC" and "sequence_".
-- Try to use a custom function instead of "print" to customise your output.

-- Task:
-- Use "zipWithM_" to number the lines of trace.

-- How would you achive customising your trace output, (e.g. numbering lines)
-- in an imperative language without modifying the
-- algrorithm itself?

-- Read the API of the Control.Monad module.

-- =========================================
-- Bad practice
-- =========================================

-- Try to make the following line the first clause of your gcdA function.
-- You have to rename func to gcdA
-- This will print a trace like you are used to in imperative languages.

func :: Integer -> Integer -> Integer
func a b | trace (show a ++ "\t" ++ show b) False = undefined

