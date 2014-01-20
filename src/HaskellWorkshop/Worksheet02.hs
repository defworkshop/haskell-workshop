module HaskellWorkshop.Worksheet02
       ( Tree(..)
       , treeSize
       , treeSize'
       , treeSize''
       , treeMax
       , treeMax'
       , treeMap
       , treeFold
       , ) where

--------------------------------------------------------------------------------
-- Define a bivariate tree. Tip: A tree is much like a list, except that it
-- has two branches
--------------------------------------------------------------------------------
data Tree a = Empty |
              Leaf a |
              Node (Tree a) a (Tree b)
              deriving (Eq, Ord, Show)



--------------------------------------------------------------------------------
-- Write a function that computes the size of the tree
--------------------------------------------------------------------------------
treeSize :: Tree a -> Int
treeSize = undefined


--------------------------------------------------------------------------------
-- Can you rewrite this function to be tail recursive?
--------------------------------------------------------------------------------
treeSize' :: Tree a -> Int
treeSize' = undefined


--------------------------------------------------------------------------------
-- Write a function that finds the maximum of a tree.
-- Hint: To compare entries they must be instances of the `Ord` typeclass
-- How do you handle an empty tree?
--------------------------------------------------------------------------------
treeMax = undefined


--------------------------------------------------------------------------------
-- Write a version of `map` for your tree
--------------------------------------------------------------------------------
treeMap = undefined


--------------------------------------------------------------------------------
-- Write a version of 'fold' for your tree
--------------------------------------------------------------------------------
treeFold = undefined


--------------------------------------------------------------------------------
-- Can you rewrite `size` and `treeMax` in terms of `fold`?
--------------------------------------------------------------------------------
treeSize'' = undefined

treeMax' = undefined
