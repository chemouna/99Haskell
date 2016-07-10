module NinetyNine.Problem5 where

import Test.QuickCheck

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- solution 2
myReverse2 :: [a] -> [a]
myReverse2 = foldl (flip (:)) []

-- solution 3
myReverse3 :: [a] -> [a]
myReverse3 xs = foldr (\x fId empty -> fId (x : empty)) id xs []

-- Test
prop_reverseRoundTrip xs = myReverse (myReverse xs) == xs
prop_reverseConcatSame xs1 xs2 = myReverse (xs1 ++ xs2) == (myReverse xs1) ++ (myReverse xs2)
