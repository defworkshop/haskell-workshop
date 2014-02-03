module HaskellWorkshop.Worksheet02
       ( Tree(..)
       , numOfElements
       , treeSize'
       , treeSize''
       , treeMax
       , treeMax'
       , treeMap
       , treeFold
       , myList
       , length'
       , length''
       ) where

--------------------------------------------------------------------------------
-- Define a bivariate tree. Tip: A tree is much like a list, except that it
-- has two branches
--------------------------------------------------------------------------------
data List a = Nil
            | Cons a (List a)

myList :: List Int
myList = Cons 1 (Cons 2 (Cons 3 Nil))


length' :: List a -> Int
length' Nil         = 0
length' (Cons _ xs) = 1 + length' xs

-- (1 + (1 + (1 + (1 + ........ (1 + 0)))))

length_tailrec :: List a -> Int
length_tailrec xs = len 0 xs
  where
    len acc Nil         = acc
    len acc (Cons _ ys) = len (acc+1) ys


length'' :: [a] -> Int
length'' []     = 0
length'' (_:xs) = 1 + length'' xs

data Tree a = Tip
            | Node (Tree a) a (Tree a)

myTree2 :: Tree Int
myTree2 = Node Tip 3 Tip


--------------------------------------------------------------------------------
-- Write a function that computes the size of the tree
--------------------------------------------------------------------------------
numOfElements :: Tree a -> Int
numOfElements Tip          = 0
numOfElements (Node l x r) = numOfElements l + 1 + numOfElements r

myTree3 :: Tree Int
myTree3 = Node (Node Tip 3 Tip) 4 (Node Tip 5 Tip)

--
--              4
--             / \
--            3   5
--

treeHeight :: Tree a -> Int
treeHeight Tip          = 0
treeHeight (Node l x r) = 1 + max leftHeight rightHeight
  where
    leftHeight  = treeHeight l
    rightHeight = treeHeight r


--------------------------------------------------------------------------------
-- Can you rewrite this function to be tail recursive?
--------------------------------------------------------------------------------
numOfElements' :: Tree a -> Int
numOfElements' = undefined


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
