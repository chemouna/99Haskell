
module NinetyNine.Problem4 where

import Data.Foldable
import Test.QuickCheck

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- solution 2
myLength2 :: [a] -> Int
myLength2 list = myLength_acc list 0
  where
    myLength_acc [] n = n
    myLength_acc (_:xs) n = myLength_acc xs (n + 1)

-- solution 3
myLength3 :: [a] -> Int
myLength3 =  foldl (\n _ -> n + 1) 0

-- solution 4
myLength4 :: [a] -> Int
myLength4 = foldr (\_ n -> n + 1) 0

-- solution 5
myLength5 :: [a] -> Int
myLength5 = foldr (\_ -> (+1)) 0

-- solution 6
myLength6 :: [a] -> Int
myLength6 = foldr ((+) . (const 1)) 0

-- solution 7
myLength7 :: [a] -> Int
myLength7 = foldr (const (+ 1)) 0

-- solution 8
myLength8 :: [a] -> Int
myLength8 =  foldl (const . (+1)) 0

-- solutions with zip

-- solution 9
myLength9 :: [a] -> Int
myLength9 xs = snd $ last $ zip xs [1..]

myLength10 :: [a] -> Int
myLength10 = snd . last . (flip zip [1..])

myLength11 :: [a] -> Int
myLength11 = fst . last . zip [1..]

-- solution 12 (with mapping)
myLength12 :: [a] -> Int
myLength12 = sum . map (\_ -> 1)


-- test
prop_myLengthTwoLists xs1 xs2 = myLength xs1 + myLength xs2 == myLength (xs1 ++ xs2)

