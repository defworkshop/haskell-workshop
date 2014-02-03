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
data Tree a = Tip
            | Node (Tree a) a (Tree a)
            deriving (Eq, Ord, Show)



--------------------------------------------------------------------------------
-- Write a function that computes the size of the tree
--------------------------------------------------------------------------------
treeSize :: Tree a -> Int
treeSize Tip          = 0
treeSize (Node l _ r) = treeSize l + 1 + treeSize r


--------------------------------------------------------------------------------
-- Can you rewrite this function to be tail recursive?
--------------------------------------------------------------------------------

-- No, this is not possible, because we need to process the size of two sub-trees.
-- However, with laziness we can still compute the size of the tree in constant
-- space:
toList :: Tree a -> [a]
toList Tip = []
toList (Node l x r) = x : toList l ++ toList r

treeSize' :: Tree a -> Int
treeSize' = length . toList


--------------------------------------------------------------------------------
-- Write a function that finds the maximum of a tree.
-- Hint: To compare entries they must be instances of the `Ord` typeclass
-- How do you handle an empty tree?
--------------------------------------------------------------------------------
treeMax :: Ord a => Tree a => Maybe a
treeMax Tip          = Nothing
treeMax (Node l x r) = Just $ maxWDef leftMax $ treeMax r
  where
    leftMax = maxWDef x (treeMax l)

    maxWDef y (Just z) = max y z
    maxWDef y Nothing  = y

--------------------------------------------------------------------------------
-- Write a version of `map` for your tree
--------------------------------------------------------------------------------
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Tip          = Tip
treeMap f (Node l x r) = Node (treeMap f l) (f x) (treeMap f r)

--------------------------------------------------------------------------------
-- Write a version of 'fold' for your tree
--------------------------------------------------------------------------------
treeFold :: (a -> b -> a) -> a -> Tree b -> a
treeFold _ acc Tip          = acc
treeFold f acc (Node l x r) = let acc_x = f acc x
                                  acc_l = treeFold f acc_x l
                              in treeFold f acc_l r


--------------------------------------------------------------------------------
-- Can you rewrite `size` and `treeMax` in terms of `fold`?
--------------------------------------------------------------------------------
treeSize'' :: Tree a -> Int
treeSize'' = treeFold (\acc _ -> acc + 1) 0

treeMax' :: Ord a => Tree a -> Maybe a
treeMax' = treeFold mmax Nothing
  where
    mmax Nothing  x        = Just x
    mmax (Just x) y        = Just $ max x y
