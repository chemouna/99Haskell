
{-# LANGUAGE TemplateHaskell #-}

module ProblemsTrees54a60 where

import Test.QuickCheck
import Test.QuickCheck.All

---------------------
-- | Tree
---------------------
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

leaf x = Branch x Empty Empty

height :: Tree a -> Int
height Empty = 0
height (Branch a l r) = 1 + (max (height l) (height r))


---------------------
-- | Problem  55
---------------------

-- Construct completely balanced binary trees:
-- abs |nb nodes (left subtree) -  nb nodes (right subtree)| <= 1

-- cbalTree 4 -> multiple permutations

-- p1: Branch 'x' (Branch 'x' Empty Empty)
--            (Branch 'x' Empty
--                        (Branch 'x' Empty Empty)),

-- p2: Branch 'x' (Branch 'x' Empty Empty)
--            (Branch 'x' (Branch 'x' Empty Empty)
--                        Empty),

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = let (q,r) = (n - 1) `quotRem` 2
      in [Branch 'x' left right | i <- [q .. q + r],
                                  left <- cbalTree i,
                                  right <- cbalTree (n - i - 1)]


return = []
main = $quickCheckAll


