{-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE UnicodeSyntax #-}

module Problem55 where

-- import Prelude.Unicode
import Test.QuickCheck
import Test.QuickCheck.All

-- Construct completely balanced binary trees:
-- abs |nb nodes (left subtree) -  nb nodes (right subtree)| <= 1

-- cbalTree 4 -> multiple permutations

-- p1: Branch 'x' (Branch 'x' Empty Empty)
--            (Branch 'x' Empty
--                        (Branch 'x' Empty Empty)),

-- p2: Branch 'x' (Branch 'x' Empty Empty)
--            (Branch 'x' (Branch 'x' Empty Empty)
--

date Tree a = Empty | Branch a (Tree a) (Tree a)
          deriving (Show, Eq)

-- cbalTree :: Int -> [Tree char]
-- cbalTree 0 = [Empty]
-- cbalTree n = let (q,r) = (n - 1) `quotRem` 2
--       in [Branch 'x' left right | i <- [q .. q + r],
--                                             left <- cbalTree i,
--                                             right <- cbalTree (n-i-1)]


return = []
main = $quickCheckAll
