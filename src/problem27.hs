{-# LANGUAGE FlexibleContexts #-}

module NinetyNine.Problem27 where

import Data.List (zipWith, permutations)

-- import Test.Hspec

-- Group the elements of a set into disjoint subsets.

-- cde = zipWith (+) [1,2] [3, 4]
-- zipWith take [12, 3, 4] (permutations ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"])

-- group2 :: [a] -> [b] -> [[b]]
-- group2 xs ys = take 5 (zipWith (\a b -> take a b) xs (permutations ys)

-- group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]


combination :: Int -> [a] -> [([a], [a])]
combination 0 xs = [([], xs)]
combination n [] = []
combination n (x:xs) = ts ++ ds
  where
    ts = [(x:ys, zs) |  (ys, zs) <- combination (n - 1) xs]
    ds = [(ys, x:zs) | (ys, zs) <- combination n xs]
