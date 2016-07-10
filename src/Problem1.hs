module NinetyNine.Problem1 where

import Test.QuickCheck
-- import Test.Hspec

myLast :: [a] -> a
myLast [] = error "No end to an empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

myLast2 :: [a] -> a
myLast2 = foldr1 (const id)

myLast3 = head . reverse

myLast4 [] = error "No end for empty lists!" 
myLast4 x = x !! (length x - 1)


-- Test
prop_lastOneElement [x] = myLast [x] == x
    where types = x::Int
