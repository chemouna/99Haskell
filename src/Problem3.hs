module NinetyNine.Problem3 where

import Test.QuickCheck

-- Find the K'th element of a list. The first element in the list is number 1.

elementAt :: [a] -> Int -> a
elementAt [] _ = error "Error, Index out of bounds"
elementAt (x:_) 1 = x
elementAt (_:xs) k 
  | k < 1 = error "Error, Index out of bounds"
  | otherwise = elementAt xs (k - 1)


elementAt2 list k = list !! (k - 1)

elementAt3 xs n
  | length xs < n = error "Index out of bounds"
  | otherwise = fst . last $ zip xs [1..n]

elementAt4 :: [a] -> Int -> a
elementAt4 [] n = error "list cannot be empty"
elementAt4 s@(x:xs) n
    | n > (length s) = error "n cannot be larger than list"
    | otherwise = if (length s) == n then x else elementAt4 xs n


-- Test
-- prop_elementAt (NonEmpty xs) = forAll (choose (1, length xs - 1)) $ \i ->
--     elementAt xs i == (xs !! i :: Int)

prop_elementAt2 xs i = (i >= 1) ==> (length xs > i) ==> elementAt xs i == xs !! i