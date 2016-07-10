

module NinetyNine.Problem26 where

import Data.List

-- Generate the combinations of K distinct objects chosen from the N elements of a list 
-- combinations 3 "abcdef"

-- map (\x -> take 3 x) (permutations "abcdef") --> i need now to filter to get
-- only unique lists

combinations :: Int -> [a] -> [[a]]
combinations n xs = filter ((n==).length) $ subsequences xs

