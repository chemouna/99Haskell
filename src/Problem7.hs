{-# LANGUAGE TemplateHaskell #-}

module NinetyNine.Problem7 where

import Data.Foldable
import Test.QuickCheck

data NestedList a = Elem a | List [NestedList a]

-- solution 1
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

-- solution 2
flatten2 :: NestedList a -> [a]
flatten2 (Elem a) = [a]
flatten2 (List (x:xs)) = flatten2 x ++ flatten2 (List xs)
flatten2 (List []) = []

-- solution 3 (using flipped version of bind)
flatten3 (Elem x) = return x
flatten3 (List x) = flatten3 =<< x

-- solution 4
flatten4 :: NestedList a -> [a]
flatten4 (Elem x ) = [x]
flatten4 (List xs) =  foldr (++) [] $ map flatten4 xs


-- Test
prop_flattenEmpty = (flatten (List [] :: NestedList ()) == [])

-- prop_flattenRoundTrip xs = group (flatten xs) =

-- flattening a list with one level should give back the same
-- but how do you give a generate with a list that has one level ?

-- Mapping length on a list (x) then taking the same produces the same result as
-- flattening the list and taking the length.

-- prop_flatten x = sum (map length x) == length (flatten x)
